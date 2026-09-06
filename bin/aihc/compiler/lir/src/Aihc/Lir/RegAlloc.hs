-- | A linear-scan register allocator over Lir functions.
--
-- The allocator is target-independent. A backend passes the registers it is
-- willing to give away and receives, for every value of the function, either
-- one of those registers or the verdict that the value stays in a frame
-- slot. Nothing else about the target enters the computation, so the ARM64
-- and the AMD64 backends share this module without a target description
-- beyond the pool itself.
--
-- The pool decides the shape of the result. Both backends offer only
-- callee-saved registers, which is what keeps the allocator this small: a
-- register the callee owns survives a call, so no interval has to be split
-- around one, and instruction selection keeps loading its operands into
-- scratch registers that are never in the pool. An allocated value therefore
-- has no fixed-register constraint anywhere in the function, and the
-- allocator needs neither pre-colored intervals nor clobber ranges.
--
-- The intervals are conservative. A value gets one contiguous interval from
-- the lowest to the highest position at which it is live, with no holes and
-- no splitting, so a value that dies and revives inside the span keeps its
-- register throughout. That costs registers on a wide function and buys
-- independence from the block order: the result is correct whatever order
-- the blocks arrive in and whatever the loops look like.
--
-- Not every value is offered a register. Saving and restoring one is itself
-- memory traffic, and a value the function touches once or twice does not
-- earn that; 'profitable' is the bar, and it weights a touch by the loops
-- that enclose it.
module Aihc.Lir.RegAlloc
  ( Allocation (..),
    allocateRegisters,
    Interval (..),
    functionIntervals,
  )
where

import Aihc.Lir.Syntax
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Where every value of one function lives.
data Allocation register = Allocation
  { -- | The values that live in a register.
    allocationRegisters :: !(Map Var register),
    -- | The values that live in a frame slot, in the order the function
    -- defines them. The backend gives each one a slot.
    allocationSpills :: ![Var],
    -- | The registers the allocator handed out, in pool order. The prologue
    -- saves these and every exit restores them.
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

-- | Assign the registers of the pool to the values of the function. The pool
-- is in preference order and holds no register that instruction selection
-- uses as a scratch register.
allocateRegisters :: (Ord register) => [register] -> Function -> Allocation register
allocateRegisters pool function =
  Allocation
    { allocationRegisters = assigned,
      allocationSpills = [var | var <- definitionOrder function, not (Map.member var assigned)],
      allocationUsed = [register | register <- pool, register `elem` Map.elems assigned]
    }
  where
    assigned = linearScan pool (filter (profitable (accessCounts function) (exitCount function)) (functionIntervals function))

-- | Every value the function defines, in the order the text defines it.
definitionOrder :: Function -> [Var]
definitionOrder function =
  map fst (functionParameters function)
    <> concat
      [ map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block)
      | block <- functionBlocks function
      ]

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
-- parameter is a write of the block that receives it.
data BlockFlow = BlockFlow
  { flowUpwardUses :: !(Set Var),
    flowDefinitions :: !(Set Var)
  }

blockFlow :: Block -> BlockFlow
blockFlow block =
  BlockFlow
    { flowUpwardUses = foldl' step (terminatorUses (blockTerminator block)) (reverse (blockInstructions block)),
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

-- | Walk the intervals in order of their start and hand out registers.
--
-- An interval that outlives another may take its register once that one has
-- expired. When nothing is free, the interval that reaches furthest goes to a
-- frame slot; it is the one whose register would sit idle the longest.
linearScan :: (Ord register) => [register] -> [Interval] -> Map Var register
linearScan pool intervals = scanState (foldl' step (ScanState [] pool Map.empty) ordered)
  where
    ordered = sortOn (\interval -> (intervalStart interval, intervalVar interval)) intervals
    step state interval =
      let expired = expire (intervalStart interval) state
       in case scanFree expired of
            register : _ -> activate interval register expired
            [] -> spill interval expired
    -- The active intervals that end before this one starts give their
    -- registers back. An interval that ends exactly where the next begins
    -- keeps its register: the two are treated as overlapping.
    expire position state =
      let (done, alive) = span (\(active, _) -> intervalEnd active < position) (scanActive state)
       in state
            { scanActive = alive,
              scanFree = [register | register <- pool, register `elem` map snd done || register `elem` scanFree state]
            }
    activate interval register state =
      state
        { scanActive = sortOn (intervalEnd . fst) ((interval, register) : scanActive state),
          scanFree = filter (/= register) (scanFree state),
          scanState = Map.insert (intervalVar interval) register (scanState state)
        }
    -- The furthest-reaching interval loses its register. It is either the
    -- one that arrived or the last of the active ones, which the active list
    -- keeps sorted by end.
    spill interval state =
      case reverse (scanActive state) of
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
