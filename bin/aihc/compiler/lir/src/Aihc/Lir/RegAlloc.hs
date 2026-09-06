-- | A linear-scan register allocator over Lir functions.
--
-- The allocator is target-independent. A backend describes the registers it
-- is willing to give away and receives, for every value of the function,
-- either one of those registers or the verdict that the value stays in a
-- frame slot.
--
-- The registers come in two classes. A volatile register is clobbered by
-- every call and costs nothing to use. A preserved register survives a C
-- call, because the C callee saves it, and is clobbered by an aihc call,
-- because an aihc function saves nothing. So a value that lives across a C
-- call takes a preserved register, a value that lives across an aihc call
-- goes to a frame slot, and everything else takes whatever is free. That is
-- the whole of the interaction between calls and registers: no interval is
-- ever split, and no register is ever pre-colored.
--
-- The intervals are conservative. A value gets one contiguous interval from
-- the lowest to the highest position at which it is live, with no holes and
-- no splitting, so a value that dies and revives inside the span keeps its
-- register throughout. That costs registers on a wide function and buys
-- independence from the block order: the result is correct whatever order
-- the blocks arrive in and whatever the loops look like.
--
-- A hint is a register the scan tries first. Parameters, call arguments,
-- call results, and returned values are hinted with the register the
-- convention puts them in. The argument of a jump and the block parameter
-- it reaches are partners: each prefers the register the other already has,
-- and failing that the register the other was hinted with. A hint that is
-- not free at the time is dropped, so hints cost nothing in correctness and
-- buy most of the moves that a convention would otherwise need.
--
-- 'allocateRegisters' is the older entry point: one pool of preserved
-- registers, no hints, and a value has to earn its register by being
-- touched often enough to pay for the save and the restores. The AMD64
-- backend still uses it.
module Aihc.Lir.RegAlloc
  ( Allocation (..),
    Registers (..),
    allocateRegisters,
    allocateRegistersFor,
    Interval (..),
    functionIntervals,
    readCounts,
  )
where

import Aihc.Lir.Syntax
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Where every value of one function lives.
data Allocation register = Allocation
  { -- | The values that live in a register.
    allocationRegisters :: !(Map Var register),
    -- | The values that live in a frame slot, in the order the function
    -- defines them. The backend gives each one a slot.
    allocationSpills :: ![Var],
    -- | The registers the allocator handed out, in pool order. The backend
    -- saves the preserved ones among them when its convention asks for it.
    allocationUsed :: ![register]
  }
  deriving (Eq, Show)

-- | The live interval of one value: the lowest and the highest position at
-- which it is live. Positions number the function in block order.
data Interval = Interval
  { intervalVar :: !Var,
    intervalStart :: !Int,
    intervalEnd :: !Int
  }
  deriving (Eq, Show)

-- | The registers a backend offers and what the calls of a function do to
-- them.
data Registers register = Registers
  { -- | The registers every call clobbers, in preference order.
    registersVolatile :: ![register],
    -- | The registers a C call preserves and an aihc call clobbers, in
    -- preference order.
    registersPreserved :: ![register],
    -- | Whether a preserved register costs the function a save and a
    -- restore. It does under the C convention, where the caller expects the
    -- register back, and it does not under the aihc convention.
    registersPreservedCost :: !Bool,
    -- | The register that carries parameter, argument, and result number
    -- @i@ under the conventions of the target, when one does.
    registersArgument :: !(Int -> Maybe register)
  }

-- | Assign the registers of the target to the values of the function. The
-- signatures resolve the convention of every direct call.
allocateRegistersFor :: (Ord register) => Registers register -> Map Symbol Signature -> Function -> Allocation register
allocateRegistersFor target signatures function =
  finish pool function (linearScan config candidates)
  where
    pool = registersVolatile target <> registersPreserved target
    config =
      Config
        { configPool = pool,
          configPreserved = Set.fromList (registersPreserved target),
          configStrictBoundary = False
        }
    counts = accessCounts function
    exits = exitCount function
    calls = callPositions signatures function
    (fixedHints, partners) = hints target function
    intervals = functionIntervals function
    starts = Map.fromList [(intervalVar interval, intervalStart interval) | interval <- intervals]
    candidates =
      [ Candidate
          { candidateInterval = interval,
            candidateReach = reach calls interval,
            candidateEarnsPreserved = not (registersPreservedCost target) || profitable counts exits interval,
            candidateHints = direct,
            candidatePartners = ours,
            candidateWeakHints = nub (concatMap (\partner -> Map.findWithDefault [] partner fixedHints) ours),
            -- A value with a hint of its own, or with a partner placed
            -- before it, has a claim on a register; it goes before the
            -- values defined at the same position that have none.
            candidateLeads = not (null direct) || any (\partner -> Map.lookup partner starts < Just (intervalStart interval)) ours
          }
      | interval <- intervals,
        let var = intervalVar interval,
        let direct = Map.findWithDefault [] var fixedHints,
        let ours = Map.findWithDefault [] var partners
      ]

-- | Assign the registers of one pool of preserved registers to the values of
-- the function. The pool is in preference order and holds no register that
-- instruction selection uses as a scratch register. A value takes a register
-- only when it is 'profitable', and a call clobbers nothing in the pool.
allocateRegisters :: (Ord register) => [register] -> Function -> Allocation register
allocateRegisters pool function =
  finish pool function (linearScan config candidates)
  where
    config =
      Config
        { configPool = pool,
          configPreserved = Set.fromList pool,
          configStrictBoundary = True
        }
    counts = accessCounts function
    exits = exitCount function
    candidates =
      [ Candidate
          { candidateInterval = interval,
            candidateReach = ReachAny,
            candidateEarnsPreserved = profitable counts exits interval,
            candidateHints = [],
            candidatePartners = [],
            candidateWeakHints = [],
            candidateLeads = False
          }
      | interval <- functionIntervals function
      ]

finish :: (Ord register) => [register] -> Function -> Map Var register -> Allocation register
finish pool function assigned =
  Allocation
    { allocationRegisters = assigned,
      allocationSpills = [var | var <- definitionOrder function, not (Map.member var assigned)],
      allocationUsed = [register | register <- pool, Set.member register used]
    }
  where
    used = Set.fromList (Map.elems assigned)

-- | Every value the function defines, in the order the text defines it.
definitionOrder :: Function -> [Var]
definitionOrder function =
  map fst (functionParameters function)
    <> concat
      [ map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block)
      | block <- functionBlocks function
      ]

-- Calls

-- | The positions of the calls of a function, by the convention of the
-- callee.
data Calls = Calls
  { callsC :: !IntSet,
    callsAihc :: !IntSet
  }

callPositions :: Map Symbol Signature -> Function -> Calls
callPositions signatures function =
  Calls
    { callsC = IntSet.fromList [position | (position, CConvention) <- calls],
      callsAihc = IntSet.fromList [position | (position, AihcConvention) <- calls]
    }
  where
    positions = blockPositions (functionBlocks function)
    calls =
      [ (position, convention)
      | block <- functionBlocks function,
        let here = positions Map.! blockLabel block,
        (position, instruction) <- zip (blockInstructionPositions here) (blockInstructions block),
        Just convention <- [callConvention (instructionOperation instruction)]
      ]
    callConvention operation =
      case operation of
        Call symbol _ -> Just (maybe AihcConvention signatureConvention (Map.lookup symbol signatures))
        CallIndirect _ _ signature -> Just (signatureConvention signature)
        _ -> Nothing

-- | Which registers an interval may take, given the calls it lives across.
-- A call at the start of the interval defines it and a call at its end
-- consumes it; neither clobbers it.
data Reach
  = ReachAny
  | ReachPreserved
  | ReachNone

reach :: Calls -> Interval -> Reach
reach calls interval
  | crosses (callsAihc calls) = ReachNone
  | crosses (callsC calls) = ReachPreserved
  | otherwise = ReachAny
  where
    crosses positions =
      maybe False (< intervalEnd interval) (IntSet.lookupGT (intervalStart interval) positions)

-- Hints

-- | The registers the convention suggests for each value, and the values
-- each value is copied to or from by a jump.
hints :: Registers register -> Function -> (Map Var [register], Map Var [Var])
hints target function = (fixed, partners)
  where
    argument = registersArgument target
    blocks = functionBlocks function
    parameters = Map.fromList [(blockLabel block, map fst (blockParameters block)) | block <- blocks]
    numbered values = [(var, register) | (index, OperandVar var) <- zip [0 ..] values, Just register <- [argument index]]
    fixed =
      Map.fromListWith
        (flip (<>))
        ( [(var, [register]) | (index, (var, _)) <- zip [0 ..] (functionParameters function), Just register <- [argument index]]
            <> [ (var, [register])
               | block <- blocks,
                 Instruction results operation <- blockInstructions block,
                 (var, register) <-
                   case operation of
                     Call _ arguments -> numbered arguments <> numbered (map OperandVar results)
                     CallIndirect _ arguments _ -> numbered arguments <> numbered (map OperandVar results)
                     _ -> []
               ]
            <> [ (var, [register])
               | block <- blocks,
                 (var, register) <-
                   case blockTerminator block of
                     TailCall _ arguments -> numbered arguments
                     TailCallIndirect _ arguments _ -> numbered arguments
                     Return values -> numbered values
                     _ -> []
               ]
        )
    pairs =
      [ (var, parameter)
      | block <- blocks,
        Target label arguments <- terminatorTargets (blockTerminator block),
        (OperandVar var, parameter) <- zip arguments (Map.findWithDefault [] label parameters)
      ]
    partners = Map.fromListWith (flip (<>)) ([(var, [parameter]) | (var, parameter) <- pairs] <> [(parameter, [var]) | (var, parameter) <- pairs])

-- | Whether a value earns the register it would take.
--
-- A value in a frame slot costs one memory access per definition and per use.
-- A value in a register costs none of those, and instead the prologue saves
-- the register once and every exit restores it. So the register pays for
-- itself once the value is touched more often than the function has exits
-- plus the one save.
--
-- A touch inside a loop happens once for every turn of the loop, so it counts
-- for more. The weight is a power of ten per loop that encloses the block,
-- which is the usual guess in the absence of a profile, and it is capped so
-- that a deep nest cannot overflow the count.
--
-- Several values that share a register pay the save and the restores once
-- between them, so a value that clears the bar alone is never a loss and a
-- register that several values share is a gain beyond what the bar counts.
profitable :: Map Var Int -> Int -> Interval -> Bool
profitable counts exits interval =
  Map.findWithDefault 0 (intervalVar interval) counts > 1 + exits

-- | The number of exits: the terminators that restore the saved registers. A
-- trap does not return, so it restores nothing.
exitCount :: Function -> Int
exitCount function =
  length
    [ ()
    | block <- functionBlocks function,
      case blockTerminator block of
        Return _ -> True
        TailCall _ _ -> True
        TailCallIndirect {} -> True
        _ -> False
    ]

-- | How often the function touches each value, weighted by the loops that
-- enclose the touch: once where it defines it, and once for every place it
-- reads it. A value read twice by one instruction counts twice, because
-- instruction selection reads it twice.
accessCounts :: Function -> Map Var Int
accessCounts function =
  Map.fromListWith
    (+)
    ( [ (var, weightOf (blockLabel block))
      | block <- functionBlocks function,
        var <-
          map fst (blockParameters block)
            <> concatMap instructionResults (blockInstructions block)
            <> concatMap (operationReads . instructionOperation) (blockInstructions block)
            <> terminatorReads (blockTerminator block)
      ]
        -- A parameter arrives before the first block.
        <> [(var, 1) | (var, _) <- functionParameters function]
    )
  where
    depths = loopDepths (functionBlocks function)
    weightOf label = 10 ^ min 3 (Map.findWithDefault 0 label depths)

-- | How many loops enclose each block. A loop is a back edge and the blocks
-- that reach it without leaving through its header, which is the natural loop
-- of the edge.
loopDepths :: [Block] -> Map Label Int
loopDepths blocks =
  Map.fromListWith
    (+)
    [ (label, 1)
    | (tail', header) <- edges,
      label <- Set.toList (naturalLoop predecessors header tail')
    ]
  where
    successors = Map.fromList [(blockLabel block, map targetLabel (terminatorTargets (blockTerminator block))) | block <- blocks]
    predecessors =
      Map.fromListWith
        (<>)
        [ (target, [source])
        | (source, targets) <- Map.toList successors,
          target <- targets
        ]
    edges = case blocks of
      [] -> []
      entry : _ -> backEdges successors (blockLabel entry)

-- | The edges that close a loop: an edge whose target is already on the path
-- the search took to reach its source.
backEdges :: Map Label [Label] -> Label -> [(Label, Label)]
backEdges successors entry = snd (visit (Set.empty, []) Set.empty entry)
  where
    visit (done, found) path label
      | Set.member label done = (done, found)
      | otherwise = foldl' step (Set.insert label done, found) (Map.findWithDefault [] label successors)
      where
        path' = Set.insert label path
        step (seen, edges) target
          | Set.member target path' = (seen, (label, target) : edges)
          | otherwise = visit (seen, edges) path' target

-- | The blocks of the natural loop of a back edge: its header, its source,
-- and everything that reaches the source without passing the header.
naturalLoop :: Map Label [Label] -> Label -> Label -> Set Label
naturalLoop predecessors header tail' = grow (Set.fromList [header, tail']) [tail']
  where
    grow seen [] = seen
    grow seen (label : rest)
      | label == header = grow seen rest
      | otherwise =
          let fresh = [source | source <- Map.findWithDefault [] label predecessors, not (Set.member source seen)]
           in grow (foldr Set.insert seen fresh) (fresh <> rest)

-- Positions

-- | The positions of one block. Every position is distinct and the positions
-- of a block are a contiguous run, so the whole block sits between its start
-- and its end.
data BlockPositions = BlockPositions
  { blockStartPosition :: !Int,
    -- | The position of each instruction, in order.
    blockInstructionPositions :: ![Int],
    blockTerminatorPosition :: !Int,
    blockEndPosition :: !Int
  }

blockPositions :: [Block] -> Map Label BlockPositions
blockPositions blocks = Map.fromList (go 1 blocks)
  where
    go _ [] = []
    go start (block : rest) =
      let instructions = zipWith (\index _ -> start + 1 + index) [0 ..] (blockInstructions block)
          terminator = start + 1 + length (blockInstructions block)
          positions =
            BlockPositions
              { blockStartPosition = start,
                blockInstructionPositions = instructions,
                blockTerminatorPosition = terminator,
                blockEndPosition = terminator + 1
              }
       in (blockLabel block, positions) : go (terminator + 2) rest

-- Liveness

-- | The values a block reads before it writes them, and the values it
-- writes. A jump argument is a read of the block that jumps, and a block
-- parameter is a write of the block that receives it, made before anything
-- in the block reads it.
data BlockFlow = BlockFlow
  { flowUpwardUses :: !(Set Var),
    flowDefinitions :: !(Set Var)
  }

blockFlow :: Block -> BlockFlow
blockFlow block =
  BlockFlow
    { flowUpwardUses = foldr (Set.delete . fst) (foldl' step (terminatorUses (blockTerminator block)) (reverse (blockInstructions block))) (blockParameters block),
      flowDefinitions = Set.fromList (map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block))
    }
  where
    step live instruction =
      Set.union
        (instructionUses instruction)
        (foldr Set.delete live (instructionResults instruction))

liveness :: [Block] -> Map Label (Set Var, Set Var)
liveness blocks = converge initial
  where
    flows = Map.fromList [(blockLabel block, blockFlow block) | block <- blocks]
    successors = Map.fromList [(blockLabel block, map targetLabel (terminatorTargets (blockTerminator block))) | block <- blocks]
    initial = Map.fromList [(blockLabel block, (Set.empty, Set.empty)) | block <- blocks]
    converge current =
      let next = foldl' update current (reverse (map blockLabel blocks))
       in if next == current then current else converge next
    update current label =
      let flow = flows Map.! label
          liveOut = Set.unions [fst (current Map.! successor) | successor <- successors Map.! label]
          liveIn = Set.union (flowUpwardUses flow) (Set.difference liveOut (flowDefinitions flow))
       in Map.insert label (liveIn, liveOut) current

-- Intervals

-- | The live interval of every value of the function.
--
-- A value is relevant at its definition, at each of its uses, at the start of
-- every block it is live into, and at the end of every block it is live out
-- of. The interval spans the lowest to the highest of those positions, which
-- covers every point at which the value is live whatever the block order.
functionIntervals :: Function -> [Interval]
functionIntervals function =
  [ Interval {intervalVar = var, intervalStart = start, intervalEnd = end}
  | (var, (start, end)) <- Map.toAscList bounds
  ]
  where
    blocks = functionBlocks function
    positions = blockPositions blocks
    live = liveness blocks
    bounds = foldl' note Map.empty relevant
    note current (var, position) = Map.insertWith merge var (position, position) current
    merge (newStart, newEnd) (oldStart, oldEnd) = (min newStart oldStart, max newEnd oldEnd)
    relevant =
      -- A parameter is defined before the first block.
      [(var, 0) | (var, _) <- functionParameters function]
        <> concat
          [ [(var, blockStartPosition here) | (var, _) <- blockParameters block]
              <> concat
                [ [(result, position) | result <- instructionResults instruction]
                    <> [(var, position) | var <- Set.toList (instructionUses instruction)]
                | (position, instruction) <- zip (blockInstructionPositions here) (blockInstructions block)
                ]
              <> [(var, blockTerminatorPosition here) | var <- Set.toList (terminatorUses (blockTerminator block))]
              <> [(var, blockStartPosition here) | var <- Set.toList liveIn]
              <> [(var, blockEndPosition here) | var <- Set.toList liveOut]
          | block <- blocks,
            let here = positions Map.! blockLabel block,
            let (liveIn, liveOut) = live Map.! blockLabel block
          ]

-- Linear scan

data Config register = Config
  { -- | Every register, in preference order.
    configPool :: ![register],
    configPreserved :: !(Set register),
    -- | Whether an interval that ends where another begins overlaps it. The
    -- older entry point keeps the two apart; the newer one lets the value an
    -- instruction consumes hand its register to the value it defines, which
    -- every instruction of a backend using it must tolerate.
    configStrictBoundary :: !Bool
  }

data Candidate register = Candidate
  { candidateInterval :: !Interval,
    candidateReach :: !Reach,
    -- | Whether the value may take a preserved register.
    candidateEarnsPreserved :: !Bool,
    -- | The registers the conventions suggest for the value itself.
    candidateHints :: ![register],
    -- | The values a jump copies this one to or from.
    candidatePartners :: ![Var],
    -- | The registers the conventions suggest for the partners.
    candidateWeakHints :: ![register],
    -- | Whether the value goes before the others defined at its position.
    candidateLeads :: !Bool
  }

-- | Whether a candidate may live in a register.
accepts :: (Ord register) => Config register -> Candidate register -> register -> Bool
accepts config candidate register =
  case candidateReach candidate of
    ReachNone -> False
    ReachPreserved -> preserved && candidateEarnsPreserved candidate
    ReachAny -> not preserved || candidateEarnsPreserved candidate
  where
    preserved = Set.member register (configPreserved config)

-- | Walk the intervals in order of their start and hand out registers.
--
-- An interval that outlives another may take its register once that one has
-- expired. A hint of the value that is free is taken first, then the
-- register of a partner already placed, then a hint of a partner, then the
-- first free register of the pool. When nothing acceptable is free, the
-- acceptable interval that reaches furthest goes to a frame slot; it is the
-- one whose register would sit idle the longest.
linearScan :: (Ord register) => Config register -> [Candidate register] -> Map Var register
linearScan config candidates = scanState (foldl' step (ScanState [] (configPool config) Map.empty) ordered)
  where
    ordered = sortOn (\candidate -> (intervalStart (candidateInterval candidate), not (candidateLeads candidate), intervalVar (candidateInterval candidate))) candidates
    step state candidate =
      let interval = candidateInterval candidate
          expired = expire (intervalStart interval) state
          preferred =
            candidateHints candidate
              <> mapMaybe (`Map.lookup` scanState expired) (candidatePartners candidate)
              <> candidateWeakHints candidate
          choices = [register | register <- preferred <> scanFree expired, register `elem` scanFree expired, accepts config candidate register]
       in case choices of
            register : _ -> activate interval register expired
            [] -> spill candidate expired
    -- The active intervals that end before this one starts give their
    -- registers back. An interval that ends exactly where the next begins
    -- does so too unless the boundary is strict, and never when it never
    -- lived past its definition: two values one instruction defines must
    -- not share.
    expire position state =
      let finished active =
            intervalEnd active < position
              || (not (configStrictBoundary config) && intervalEnd active == position && intervalStart active < intervalEnd active)
          (done, alive) = span (finished . fst) (scanActive state)
       in state
            { scanActive = alive,
              scanFree = [register | register <- configPool config, register `elem` map snd done || register `elem` scanFree state]
            }
    activate interval register state =
      state
        { scanActive = sortOn (intervalEnd . fst) ((interval, register) : scanActive state),
          scanFree = filter (/= register) (scanFree state),
          scanState = Map.insert (intervalVar interval) register (scanState state)
        }
    -- The furthest-reaching acceptable interval loses its register. The
    -- active list is sorted by end, so it is the last acceptable one.
    spill candidate state =
      let interval = candidateInterval candidate
       in case reverse [(active, register) | (active, register) <- scanActive state, accepts config candidate register] of
            (victim, register) : _
              | intervalEnd victim > intervalEnd interval ->
                  activate
                    interval
                    register
                    state
                      { scanActive = filter ((/= intervalVar victim) . intervalVar . fst) (scanActive state),
                        scanFree = register : scanFree state,
                        scanState = Map.delete (intervalVar victim) (scanState state)
                      }
            _ -> state

data ScanState register = ScanState
  { -- | The intervals holding a register, sorted by their end.
    scanActive :: ![(Interval, register)],
    -- | The registers nothing holds, in pool order.
    scanFree :: ![register],
    scanState :: !(Map Var register)
  }

-- Uses

-- | How many times the function reads each value, unweighted.
readCounts :: Function -> Map Var Int
readCounts function =
  Map.fromListWith
    (+)
    [ (var, 1)
    | block <- functionBlocks function,
      var <- concatMap (operationReads . instructionOperation) (blockInstructions block) <> terminatorReads (blockTerminator block)
    ]

instructionUses :: Instruction -> Set Var
instructionUses = Set.fromList . operationReads . instructionOperation

-- | Every read of a value by one operation, in order and with repeats.
operationReads :: Operation -> [Var]
operationReads operation =
  case operation of
    Binary _ _ left right -> operands [left, right]
    Unary _ _ value -> operands [value]
    Wide _ _ left right -> operands [left, right]
    Compare _ _ left right -> operands [left, right]
    FloatBinary _ _ left right -> operands [left, right]
    FloatUnary _ _ value -> operands [value]
    Convert _ _ value _ -> operands [value]
    PtrToInt value -> operands [value]
    PtrFromInt value -> operands [value]
    Select _ condition left right -> operands [condition, left, right]
    Load _ address _ -> operands [addressBase address]
    Store _ value address _ -> operands [value, addressBase address]
    PtrAdd base offset -> operands [base, offset]
    StackAlloc _ _ -> []
    GlobalGet _ -> []
    GlobalSet _ value -> operands [value]
    Call _ arguments -> operands arguments
    CallIndirect callee arguments _ -> operands (callee : arguments)

terminatorUses :: Terminator -> Set Var
terminatorUses = Set.fromList . terminatorReads

-- | Every read of a value by one terminator, in order and with repeats.
terminatorReads :: Terminator -> [Var]
terminatorReads terminator =
  case terminator of
    Jump target -> operands (targetArguments target)
    Branch condition whenTrue whenFalse -> operands (condition : targetArguments whenTrue <> targetArguments whenFalse)
    Switch _ scrutinee cases fallback ->
      operands
        ( scrutinee
            : concatMap (targetArguments . switchCaseTarget) cases
              <> concatMap targetArguments fallback
        )
    Return values -> operands values
    TailCall _ arguments -> operands arguments
    TailCallIndirect callee arguments _ -> operands (callee : arguments)
    Trap _ -> []

operands :: [Operand] -> [Var]
operands values = [var | OperandVar var <- values]
