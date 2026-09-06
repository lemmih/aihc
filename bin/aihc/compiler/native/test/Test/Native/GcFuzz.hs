{-# LANGUAGE LambdaCase #-}

-- | Fuzz tests for the semispace collector.
--
-- No source fixture can drive this test. The collector's input is a heap
-- state, and a compiled program reaches only the states its own evaluation
-- produces. The test therefore builds random heaps directly through the
-- runtime interface. A model in this module predicts the result of every
-- collection, and a C driver reports what the collector did.
--
-- A script is a list of epochs. Each epoch allocates one block of objects,
-- then changes arrays, thunks, static objects, and roots, and then collects.
-- The driver keeps a table from object identity to address, so later epochs
-- name objects that an earlier collection moved.
module Test.Native.GcFuzz
  ( tests,
  )
where

import Aihc.Cli.Runtime (RuntimeBuild (..))
import Aihc.Native (NativeTarget (Llvm), RuntimeGarbageCollector (..), backendCompiler)
import Aihc.Testing.RuntimeArchive (cachedRuntimeArchive)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, SomeException, throwIO, try)
import Control.Monad (forM, replicateM, unless)
import Data.Bits (shiftL, (.|.))
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word64)
import Hedgehog (Gen, Property, annotate, classify, evalIO, failure, forAllWith, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (readHex, showHex)
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hFlush, hGetContents, hGetLine, hPutStr, hSetBuffering)
import System.IO.Error (tryIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, readProcessWithExitCode, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

-- * Configuration

-- | One runtime configuration of the collector.
data Config = Config
  { cfgName :: String,
    cfgArgs :: [String],
    -- | The @-Zs@ option: static objects live only through reference tables.
    cfgStaticRoots :: Bool
  }

configs :: [Config]
configs =
  [ Config "default" [] False,
    Config "-Zs" ["+RTS", "-Zs", "-RTS"] True
  ]

tests :: TestTree
tests =
  withResource compileDriver (removeDirectoryRecursive . fst) $ \getBuild ->
    testGroup
      "semispace collector fuzz"
      [ withResource (newDriver getBuild config) stopDriver $ \getDriver ->
          testGroup
            (cfgName config)
            [ testProperty "collects random heaps" (prop_collect config getDriver),
              testCase "forwards the target of an evaluated static thunk" (checkScript config getDriver evaluatedStaticScript)
            ]
      | config <- configs
      ]

-- | Evaluate a static thunk into a heap object that only the thunk keeps
-- alive, then force a collection. The thunk must reach the moved object
-- afterwards. By default the runtime keeps every evaluated static object.
-- Under @-Zs@ the published table names the thunk.
--
-- This fixed script is a unit test for the same reason as the property
-- above: no source fixture can force a collection at a chosen heap state, so
-- the script drives the runtime directly.
evaluatedStaticScript :: [Command]
evaluatedStaticScript =
  [ CMachine 0 0 64,
    CSrt 0 [0] [],
    CCurrentSrt (Just 0),
    CReserve 2,
    CNew 1 KNode [] Nothing,
    CSUpdate 0 (VHeap 1),
    CFill 0,
    CCollect
  ]

-- | Run one fixed script and compare the driver's reports with the model.
checkScript :: Config -> IO Driver -> [Command] -> IO ()
checkScript config getDriver script = do
  driver <- getDriver
  result <- runScript driver (renderScript script)
  output <- either assertFailure pure result
  reports <- either assertFailure pure (parseReports output)
  let problems = replay config script reports
  assertBool ("driver output:\n" <> unlines output <> unlines problems) (null problems)
  assertBool "the script reports one collection" (Map.size reports == 1)

prop_collect :: Config -> IO Driver -> Property
prop_collect config getDriver = property $ do
  script <- forAllWith renderScript (genScript config)
  driver <- evalIO getDriver
  result <- evalIO (runScript driver (renderScript script))
  case result of
    Left message -> do
      annotate message
      failure
    Right output ->
      case parseReports output of
        Left message -> do
          annotate ("driver output:\n" <> unlines output)
          annotate message
          failure
        Right reports -> do
          let problems = replay config script reports
          classify (fromString "several collections") (Map.size reports > 1)
          classify (fromString "collection at a reservation") (or [Map.member index reports | (index, CReserve _) <- zip [0 ..] script])
          classify (fromString "thunk update") (or [True | CUpdate {} <- script])
          classify (fromString "blackhole") (or [True | CBlackhole _ <- script])
          classify (fromString "static thunk update") (or [True | CSUpdate {} <- script])
          classify (fromString "decoy word") (or [True | CSet _ _ (VDecoy _) <- script])
          classify (fromString "more than four survivors") (any ((> 4) . Map.size . rObjects) (Map.elems reports))
          classify (fromString "more than sixteen survivors") (any ((> 16) . Map.size . rObjects) (Map.elems reports))
          classify (fromString "stale static object") (or [True | CSUpdate {} <- script] && cfgStaticRoots config)
          unless (null problems) $ do
            annotate ("driver output:\n" <> unlines output)
            annotate (unlines problems)
            failure

-- * Model

type Id = Int

data Kind = KNode | KClosure | KThunk | KPartial
  deriving (Eq, Show)

-- | A slot value. A decoy is the address of a live object written into a
-- non-pointer field, so the collector must leave it alone.
data Value = VNull | VHeap Id | VStatic Int | VWord Word64 | VDecoy Id
  deriving (Eq, Show)

data Object
  = Object
      { oKind :: Kind,
        oPointers :: [Bool],
        oFields :: [Value],
        oSrt :: Maybe Int,
        oBlackholed :: Bool
      }
  | Array [Value] (Maybe Int)
  | Ind Value
  deriving (Eq, Show)

-- | The state of one static thunk slot. A stale slot was an evaluated static
-- object that a collection under @-Zs@ did not mark. Its target is invalid,
-- and nothing may reach the slot again.
data StaticThunk = SThunk | SInd Value | SStale
  deriving (Eq, Show)

data ThreadSlot = SlotFunction | SlotContinuation | SlotValue
  deriving (Eq, Show)

data Model = Model
  { mHeap :: Map Id Object,
    mNextId :: Id,
    mGlobals :: [Value],
    mRoots :: [Value],
    -- | Stable-name referents, newest first.
    mStable :: [Value],
    mMvars :: [Maybe Value],
    mThreadFunction :: Value,
    mThreadContinuation :: Value,
    mThreadValue :: Maybe Value,
    -- | Thunks under evaluation, newest first.
    mBlackholes :: [Id],
    mStaticThunks :: [StaticThunk],
    mStaticThunkSrts :: [Maybe Int],
    mStaticNodes :: [[Value]],
    mStaticNodeSrts :: [Maybe Int],
    mSrts :: Map Int ([Int], [Int]),
    mCurrentSrt :: Maybe Int
  }
  deriving (Show)

staticThunkCount, staticNodeCount, staticNullaryCount, staticRootedCount, staticCount, staticNodeFields :: Int
staticThunkCount = 8
staticNodeCount = 4
staticNullaryCount = 4
staticRootedCount = staticThunkCount + staticNodeCount
staticCount = staticRootedCount + staticNullaryCount
staticNodeFields = 2

emptyModel :: Model
emptyModel =
  Model
    { mHeap = Map.empty,
      mNextId = 1,
      mGlobals = [],
      mRoots = [],
      mStable = [],
      mMvars = [],
      mThreadFunction = VNull,
      mThreadContinuation = VNull,
      mThreadValue = Nothing,
      mBlackholes = [],
      mStaticThunks = replicate staticThunkCount SThunk,
      mStaticThunkSrts = replicate staticThunkCount Nothing,
      mStaticNodes = replicate staticNodeCount (replicate staticNodeFields VNull),
      mStaticNodeSrts = replicate staticNodeCount Nothing,
      mSrts = Map.empty,
      mCurrentSrt = Nothing
    }

objectWords :: Object -> Int
objectWords (Object kind _ fields _ _)
  | kind == KThunk = 1 + max 1 (length fields)
  | otherwise = 1 + length fields
objectWords (Array elements _) = 2 + length elements
objectWords (Ind _) = 2

-- | Follow heap indirections to the value they name.
resolve :: Model -> Value -> Value
resolve model = go (1000 :: Int)
  where
    go 0 _ = error "indirection chain is too long"
    go fuel value = case value of
      VHeap identity
        | Just (Ind target) <- Map.lookup identity (mHeap model) -> go (fuel - 1) target
      _ -> value

setAt :: Int -> a -> [a] -> [a]
setAt index value list = [if position == index then value else old | (position, old) <- zip [0 ..] list]

-- * Commands

data Command
  = CMachine Int Int Int
  | CSrt Int [Int] [Int]
  | CCurrentSrt (Maybe Int)
  | CMvars Int
  | CFill Int
  | CReserve Int
  | CNew Id Kind [Bool] (Maybe Int)
  | CArray Id Int (Maybe Int)
  | CSet Id Int Value
  | CUpdate Id Value
  | CBlackhole Id
  | CUnblackhole Id Value
  | CSUpdate Int Value
  | CSSet Int Int Value
  | CSSrt Int (Maybe Int)
  | CGlobal Int Value
  | CRoot Int Value
  | CStable Value
  | CMvarPut Int Value
  | CMvarTake Int
  | CThread ThreadSlot Value
  | CCollect
  deriving (Eq, Show)

renderValue :: Value -> String
renderValue VNull = "n"
renderValue (VHeap identity) = 'h' : show identity
renderValue (VStatic slot) = 's' : show slot
renderValue (VWord word) = 'w' : showHex word ""
renderValue (VDecoy identity) = 'a' : show identity

renderSrt :: Maybe Int -> String
renderSrt = maybe "-1" show

renderCommand :: Command -> String
renderCommand command = unwords $ case command of
  CMachine globals roots bytes -> ["machine", show globals, show roots, show bytes]
  CSrt index objects children -> ["srt", show index, show (length objects), show (length children)] <> map (('s' :) . show) objects <> map show children
  CCurrentSrt srt -> ["current_srt", renderSrt srt]
  CMvars count -> ["mvars", show count]
  CFill keep -> ["fill", show keep]
  CReserve count -> ["reserve", show count]
  CNew identity kind pointers srt -> ["new", show identity, kindName kind, if null pointers then "-" else map (\p -> if p then '1' else '0') pointers, renderSrt srt]
  CArray identity count srt -> ["array", show identity, show count, renderSrt srt]
  CSet identity index value -> ["set", show identity, show index, renderValue value]
  CUpdate identity value -> ["update", show identity, renderValue value]
  CBlackhole identity -> ["blackhole", show identity]
  CUnblackhole identity value -> ["unblackhole", show identity, renderValue value]
  CSUpdate slot value -> ["supdate", show slot, renderValue value]
  CSSet slot index value -> ["sset", show slot, show index, renderValue value]
  CSSrt slot srt -> ["ssrt", show slot, renderSrt srt]
  CGlobal index value -> ["global", show index, renderValue value]
  CRoot index value -> ["root", show index, renderValue value]
  CStable value -> ["stable", renderValue value]
  CMvarPut index value -> ["mvar_put", show index, renderValue value]
  CMvarTake index -> ["mvar_take", show index]
  CThread slot value -> ["thread", threadSlotName slot, renderValue value]
  CCollect -> ["collect"]

kindName :: Kind -> String
kindName KNode = "node"
kindName KClosure = "closure"
kindName KThunk = "thunk"
kindName KPartial = "partial"

threadSlotName :: ThreadSlot -> String
threadSlotName SlotFunction = "function"
threadSlotName SlotContinuation = "continuation"
threadSlotName SlotValue = "value"

renderScript :: [Command] -> String
renderScript = unlines . map renderCommand

-- | Apply the effect of one command. Collections are applied separately when
-- the driver reports them.
applyCommand :: Command -> Model -> Model
applyCommand command model = case command of
  CMachine globals roots _ -> emptyModel {mGlobals = replicate globals VNull, mRoots = replicate roots VNull}
  CSrt index objects children -> model {mSrts = Map.insert index (objects, children) (mSrts model)}
  CCurrentSrt srt -> model {mCurrentSrt = srt}
  CMvars count -> model {mMvars = replicate count Nothing}
  CFill _ -> model
  CReserve _ -> model
  CNew identity kind pointers srt ->
    insertObject identity (Object kind pointers [if p then VNull else VWord 0 | p <- pointers] srt False)
  CArray identity count srt -> insertObject identity (Array (replicate count VNull) srt)
  CSet identity index value -> adjustObject identity $ \case
    Object kind pointers fields srt blackholed -> Object kind pointers (setAt index value fields) srt blackholed
    Array elements srt -> Array (setAt index value elements) srt
    Ind _ -> error "set on an indirection"
  CUpdate identity value -> adjustObject identity (const (Ind value))
  CBlackhole identity ->
    (adjustObject identity blackhole) {mBlackholes = identity : mBlackholes model}
  CUnblackhole identity value ->
    (adjustObject identity (const (Ind value))) {mBlackholes = filter (/= identity) (mBlackholes model)}
  CSUpdate slot value -> model {mStaticThunks = setAt slot (SInd value) (mStaticThunks model)}
  CSSet slot index value ->
    let node = slot - staticThunkCount
     in model {mStaticNodes = setAt node (setAt index value (mStaticNodes model !! node)) (mStaticNodes model)}
  CSSrt slot srt
    | slot < staticThunkCount -> model {mStaticThunkSrts = setAt slot srt (mStaticThunkSrts model)}
    | otherwise -> model {mStaticNodeSrts = setAt (slot - staticThunkCount) srt (mStaticNodeSrts model)}
  CGlobal index value -> model {mGlobals = setAt index value (mGlobals model)}
  CRoot index value -> model {mRoots = setAt index value (mRoots model)}
  CStable value
    | value `elem` mStable model -> model
    | otherwise -> model {mStable = value : mStable model}
  CMvarPut index value -> model {mMvars = setAt index (Just value) (mMvars model)}
  CMvarTake index -> model {mMvars = setAt index Nothing (mMvars model)}
  CThread SlotFunction value -> model {mThreadFunction = value}
  CThread SlotContinuation value -> model {mThreadContinuation = value}
  CThread SlotValue value -> model {mThreadValue = Just value}
  CCollect -> model
  where
    insertObject identity object =
      model {mHeap = Map.insert identity object (mHeap model), mNextId = max (mNextId model) (identity + 1)}
    blackhole (Object kind pointers fields srt _) = Object kind pointers fields srt True
    blackhole _ = error "blackhole on an object that is not a thunk"
    adjustObject identity change = model {mHeap = Map.adjust change identity (mHeap model)}

applyCommands :: [Command] -> Model -> Model
applyCommands commands model = foldl' (flip applyCommand) model commands

-- * Liveness

data Live = Live
  { liveHeap :: Set Id,
    liveStatics :: Set Int
  }

data Item = IHeap Id | IStatic Int | ISrt Int

-- | Decide what one collection keeps: the heap objects it copies and the
-- static objects it marks.
--
-- The collector has no list of static objects. By default it starts from the
-- evaluated static thunks, which the runtime records when they are updated,
-- and it marks any other static object only when something points at it.
-- Under @-Zs@ it starts from the published reference table instead.
liveness :: Config -> Model -> Live
liveness config model = go initial (Live Set.empty Set.empty) Set.empty
  where
    rootValues =
      mGlobals model
        <> mRoots model
        <> mStable model
        <> catMaybes (mMvars model)
        <> [mThreadFunction model, mThreadContinuation model]
        <> maybe [] pure (mThreadValue model)
        <> map VHeap (mBlackholes model)
    staticStart
      | cfgStaticRoots config = maybe [] (pure . ISrt) (mCurrentSrt model)
      | otherwise = [IStatic slot | (slot, SInd _) <- zip [0 ..] (mStaticThunks model)]
    initial = concatMap fromValue rootValues <> staticStart
    fromValue value = case resolve model value of
      VHeap identity -> [IHeap identity]
      VStatic slot | slot < staticRootedCount -> [IStatic slot]
      _ -> []
    fromSrt = maybe [] (pure . ISrt)
    go [] live _ = live
    go (item : rest) live seenSrts = case item of
      IHeap identity
        | Set.member identity (liveHeap live) -> go rest live seenSrts
        | otherwise ->
            let object = fromMaybe (error "live object is not in the model") (Map.lookup identity (mHeap model))
                children = case object of
                  Object _ pointers fields _ _ -> concatMap fromValue [field | (True, field) <- zip pointers fields] <> fromSrt (oSrt object)
                  Array elements srt -> concatMap fromValue elements <> fromSrt srt
                  Ind _ -> error "resolved value names an indirection"
             in go (children <> rest) live {liveHeap = Set.insert identity (liveHeap live)} seenSrts
      IStatic slot
        | Set.member slot (liveStatics live) -> go rest live seenSrts
        | otherwise ->
            let children
                  | slot < staticThunkCount = case mStaticThunks model !! slot of
                      SThunk -> fromSrt (mStaticThunkSrts model !! slot)
                      SInd target -> fromValue target
                      SStale -> error "a stale static object became live"
                  | otherwise =
                      let node = slot - staticThunkCount
                       in concatMap fromValue (mStaticNodes model !! node) <> fromSrt (mStaticNodeSrts model !! node)
             in go (children <> rest) live {liveStatics = Set.insert slot (liveStatics live)} seenSrts
      ISrt index
        | Set.member index seenSrts -> go rest live seenSrts
        | otherwise ->
            let (objects, children) = fromMaybe (error "reference table is not defined") (Map.lookup index (mSrts model))
             in go (map IStatic objects <> map ISrt children <> rest) live (Set.insert index seenSrts)

-- | Apply one collection to the model.
collectModel :: Config -> Model -> Model
collectModel config model =
  model
    { mHeap = Map.mapWithKey resolveObject (Map.restrictKeys (mHeap model) (liveHeap live)),
      mGlobals = map r (mGlobals model),
      mRoots = map r (mRoots model),
      mStable = map r (mStable model),
      mMvars = map (fmap r) (mMvars model),
      mThreadFunction = r (mThreadFunction model),
      mThreadContinuation = r (mThreadContinuation model),
      mThreadValue = fmap r (mThreadValue model),
      mStaticThunks = zipWith updateStatic [0 ..] (mStaticThunks model)
    }
  where
    live = liveness config model
    r = resolve model
    resolveObject _ object = case object of
      Object kind pointers fields srt blackholed -> Object kind pointers (zipWith (\p v -> if p then r v else v) pointers fields) srt blackholed
      Array elements srt -> Array (map r elements) srt
      Ind _ -> error "an indirection survived in the model"
    updateStatic slot state
      | Set.member slot (liveStatics live) = case state of
          SInd target -> SInd (r target)
          other -> other
      | otherwise = case state of
          SInd _
            | cfgStaticRoots config -> SStale
          other -> other

-- * Reports

data RValue = RNull | RHeap Id | RStatic Int | RWord Word64 | ROld | RForeign Word64
  deriving (Eq, Show)

data RStatic = RThunk | RInd RValue | RNode [RValue]
  deriving (Eq, Show)

data Report = Report
  { rLive :: Int,
    rCapacity :: Int,
    rTarget :: Int,
    rOldCapacity :: Int,
    rRequired :: Int,
    rObjects :: Map Id (String, [RValue]),
    rGlobals :: [RValue],
    rRoots :: [RValue],
    rStable :: [RValue],
    rMvars :: [Maybe RValue],
    rThread :: [(String, RValue)],
    rBlackholes :: [RValue],
    rStatics :: Map Int RStatic,
    rViolations :: [String]
  }
  deriving (Show)

emptyReport :: Report
emptyReport = Report 0 0 0 0 0 Map.empty [] [] [] [] [] [] Map.empty []

parseRValue :: String -> Either String RValue
parseRValue token = case token of
  "n" -> Right RNull
  'h' : rest -> RHeap <$> readNumber rest
  's' : rest -> RStatic <$> readNumber rest
  'w' : rest -> RWord <$> readHexWord rest
  "o" -> Right ROld
  'x' : rest -> RForeign <$> readHexWord rest
  _ -> Left ("invalid report value " <> token)

readNumber :: String -> Either String Int
readNumber text = case reads text of
  [(value, "")] -> Right value
  _ -> Left ("invalid number " <> text)

readHexWord :: String -> Either String Word64
readHexWord text = case readHex text of
  [(value, "")] -> Right value
  _ -> Left ("invalid hex word " <> text)

-- | Parse the driver's output into reports keyed by command index.
parseReports :: [String] -> Either String (Map Int Report)
parseReports = go Map.empty
  where
    go reports [] = Right reports
    go reports (line : rest) = case words line of
      ["collection", index] -> do
        command <- readNumber index
        (report, remaining) <- block emptyReport rest
        go (Map.insert command report reports) remaining
      _ -> Left ("unexpected driver line " <> line)
    block _ [] = Left "report without end"
    block report (line : rest) = case words line of
      ["endcollection"] -> Right (report, rest)
      ["space", live, capacity, target, old, required] -> do
        values <- traverse readNumber [live, capacity, target, old, required]
        case values of
          [l, c, t, o, q] -> block report {rLive = l, rCapacity = c, rTarget = t, rOldCapacity = o, rRequired = q} rest
          _ -> Left "invalid space line"
      "obj" : identity : kind : _count : values -> do
        key <- readNumber identity
        parsed <- traverse parseRValue values
        block report {rObjects = Map.insert key (kind, parsed) (rObjects report)} rest
      ["global", _, value] -> do
        parsed <- parseRValue value
        block report {rGlobals = rGlobals report <> [parsed]} rest
      ["root", _, value] -> do
        parsed <- parseRValue value
        block report {rRoots = rRoots report <> [parsed]} rest
      ["stable", value] -> do
        parsed <- parseRValue value
        block report {rStable = rStable report <> [parsed]} rest
      ["mvar", _, "empty"] -> block report {rMvars = rMvars report <> [Nothing]} rest
      ["mvar", _, "full", value] -> do
        parsed <- parseRValue value
        block report {rMvars = rMvars report <> [Just parsed]} rest
      ["thread", slot, value] -> do
        parsed <- parseRValue value
        block report {rThread = rThread report <> [(slot, parsed)]} rest
      ["blackhole", value] -> do
        parsed <- parseRValue value
        block report {rBlackholes = rBlackholes report <> [parsed]} rest
      ["static", slot, "thunk"] -> insertStatic slot RThunk
      ["static", slot, "ind", value] -> parseRValue value >>= insertStatic slot . RInd
      "static" : slot : "node" : values -> traverse parseRValue values >>= insertStatic slot . RNode
      "violation" : message -> block report {rViolations = rViolations report <> [unwords message]} rest
      _ -> Left ("unexpected report line " <> line)
      where
        insertStatic slot state = do
          key <- readNumber slot
          block report {rStatics = Map.insert key state (rStatics report)} rest

-- * Replay

-- | Run the script against the model and check every reported collection.
replay :: Config -> [Command] -> Map Int Report -> [String]
replay config script reports = go (zip [0 ..] script) emptyModel 0 <> extra
  where
    extra = ["report for command " <> show index <> " which is not in the script" | index <- Map.keys reports, index >= length script]
    go [] _ _ = []
    go ((index, command) : rest) model capacity = case Map.lookup index reports of
      Nothing
        | CCollect <- command -> ("command " <> show index <> ": collect did not report a collection") : go rest model capacity
        | otherwise -> go rest (applyCommand command model) (capacityAfter command capacity)
      Just report
        | collects command ->
            let expected = collectModel config model
                problems = checkReport expected capacity report
             in map (\p -> "command " <> show index <> ": " <> p) problems
                  <> go rest (applyCommand command expected) (rCapacity report)
        | otherwise -> ("command " <> show index <> ": collection at a command that cannot collect") : go rest (applyCommand command model) capacity
    collects (CReserve _) = True
    collects CCollect = True
    collects _ = False
    capacityAfter (CMachine _ _ bytes) _ = bytes
    capacityAfter _ capacity = capacity

matchesValue :: Value -> RValue -> Bool
matchesValue VNull RNull = True
matchesValue (VHeap expected) (RHeap actual) = expected == actual
matchesValue (VStatic expected) (RStatic actual) = expected == actual
matchesValue (VWord expected) (RWord actual) = expected == actual
matchesValue (VDecoy _) (RWord _) = True
matchesValue _ _ = False

checkValues :: String -> [Value] -> [RValue] -> [String]
checkValues what expected actual
  | length expected /= length actual = [what <> ": expected " <> show expected <> " but the driver reported " <> show actual]
  | and (zipWith matchesValue expected actual) = []
  | otherwise = [what <> ": expected " <> show expected <> " but the driver reported " <> show actual]

-- | Compare one reported collection with the collected model. The model
-- before the collection gives the capacity the driver must have kept.
checkReport :: Model -> Int -> Report -> [String]
checkReport expected capacityBefore report =
  map ("violation: " <>) (rViolations report)
    <> spaceProblems
    <> objectProblems
    <> checkValues "globals" (mGlobals expected) (rGlobals report)
    <> checkValues "roots" (mRoots expected) (rRoots report)
    <> checkValues "stable names" (mStable expected) (rStable report)
    <> mvarProblems
    <> threadProblems
    <> checkValues "blackholes" (map VHeap (mBlackholes expected)) (rBlackholes report)
    <> staticProblems
  where
    liveBytes = 8 * sum (map objectWords (Map.elems (mHeap expected)))
    occupied = rLive report + rRequired report
    spaceProblems =
      ["live bytes: expected " <> show liveBytes <> " but the driver reported " <> show (rLive report) | rLive report /= liveBytes]
        <> ["capacity " <> show (rCapacity report) <> " is below live data plus reservation " <> show occupied | rCapacity report < occupied]
        <> ["target " <> show (rTarget report) <> " is below twice the occupied bytes " <> show occupied | rTarget report < 2 * occupied]
        <> ["old capacity: expected " <> show capacityBefore <> " but the driver reported " <> show (rOldCapacity report) | rOldCapacity report /= capacityBefore]
    expectedObjects = Map.map expectedObject (mHeap expected)
    expectedObject object = case object of
      Object kind pointers fields _ blackholed -> (if blackholed then "blackhole" else kindName kind, pointers, fields)
      Array elements _ -> ("array", map (const True) elements, elements)
      Ind _ -> ("indirection", [], [])
    objectProblems =
      ["object " <> show identity <> " survived but is not live in the model" | identity <- Map.keys (rObjects report), not (Map.member identity expectedObjects)]
        <> concatMap objectProblem (Map.toList expectedObjects)
    objectProblem (identity, (kind, _, fields)) = case Map.lookup identity (rObjects report) of
      Nothing -> ["object " <> show identity <> " is live in the model but did not survive"]
      Just (actualKind, actualFields) ->
        ["object " <> show identity <> ": expected kind " <> kind <> " but the driver reported " <> actualKind | actualKind /= kind]
          <> checkValues ("object " <> show identity <> " fields") fields actualFields
    mvarProblems
      | length (mMvars expected) /= length (rMvars report) = ["mvar count differs"]
      | otherwise = concat (zipWith3 mvarProblem [0 :: Int ..] (mMvars expected) (rMvars report))
    mvarProblem index expectedMvar actualMvar = case (expectedMvar, actualMvar) of
      (Nothing, Nothing) -> []
      (Just value, Just actual) -> checkValues ("mvar " <> show index) [value] [actual]
      _ -> ["mvar " <> show index <> ": expected " <> show expectedMvar <> " but the driver reported " <> show actualMvar]
    threadExpected =
      [("function", mThreadFunction expected), ("continuation", mThreadContinuation expected)]
        <> maybe [] (\v -> [("value", v)]) (mThreadValue expected)
    threadProblems
      | map fst threadExpected /= map fst (rThread report) = ["thread slots: expected " <> show threadExpected <> " but the driver reported " <> show (rThread report)]
      | otherwise = checkValues "thread slots" (map snd threadExpected) (map snd (rThread report))
    staticProblems = concatMap thunkProblem (zip [0 ..] (mStaticThunks expected)) <> concatMap nodeProblem (zip [0 ..] (mStaticNodes expected))
    thunkProblem (slot, state) = case (state, Map.lookup slot (rStatics report)) of
      (_, Nothing) -> ["static " <> show slot <> " is missing from the report"]
      (SStale, _) -> []
      (SThunk, Just RThunk) -> []
      (SInd value, Just (RInd actual)) -> checkValues ("static " <> show slot) [value] [actual]
      (_, Just actual) -> ["static " <> show slot <> ": expected " <> show state <> " but the driver reported " <> show actual]
    nodeProblem (node, fields) = case Map.lookup (staticThunkCount + node) (rStatics report) of
      Just (RNode actual) -> checkValues ("static node " <> show node) fields actual
      other -> ["static node " <> show node <> ": expected fields but the driver reported " <> show other]

-- * Generation

-- | Knobs that shape one script. Each script draws its own profile, so the
-- suite covers sparse and dense heaps, pointer-heavy and word-heavy objects,
-- and frequent and rare collections.
data Profile = Profile
  { pBlockMax :: Int,
    pFieldMax :: Int,
    pArrayMax :: Int,
    pPointerPercent :: Int,
    pNullPercent :: Int,
    pStaticPercent :: Int,
    pDecoyPercent :: Int,
    pArrayWeight :: Int,
    pThunkWeight :: Int,
    pOpsMax :: Int,
    pCollectPercent :: Int,
    pEpochMax :: Int,
    pFillPercent :: Int,
    -- | How often a root slot is pointed at a fresh object of the block.
    pRootPercent :: Int
  }

genProfile :: Gen Profile
genProfile =
  Profile
    <$> Gen.element [1, 2, 4, 8, 16, 32, 64, 128]
    <*> Gen.element [0, 1, 2, 3, 6]
    <*> Gen.element [0, 1, 2, 4, 8, 32]
    <*> Gen.element [0, 25, 50, 75, 100]
    <*> Gen.element [0, 10, 30, 60]
    <*> Gen.element [0, 10, 30]
    <*> Gen.element [0, 20]
    <*> Gen.element [0, 1, 3]
    <*> Gen.element [0, 1, 3]
    <*> Gen.element [0, 4, 8, 16, 32]
    <*> Gen.element [0, 30, 60, 100]
    <*> Gen.element [1, 2, 4, 6]
    <*> Gen.element [0, 30, 100]
    <*> Gen.element [0, 30, 70]

percent :: Int -> Gen Bool
percent p = (< p) <$> Gen.int (Range.constant 0 99)

-- | A weighted choice that drops alternatives with weight zero. Hedgehog's
-- 'Gen.frequency' can select a zero-weight alternative while it shrinks.
weighted :: [(Int, Gen a)] -> Gen a
weighted alternatives = Gen.frequency [(weight, gen) | (weight, gen) <- alternatives, weight > 0]

elementOr :: a -> [a] -> Gen a
elementOr fallback [] = pure fallback
elementOr _ list = Gen.element list

genScript :: Config -> Gen [Command]
genScript config = do
  profile <- genProfile
  globals <- Gen.int (Range.linear 0 8)
  roots <- Gen.int (Range.linear 0 8)
  spaceWords <- Gen.element [8, 16, 32, 64, 256, 1024, 8192]
  mvarCount <- Gen.int (Range.linear 0 3)
  srtCount <- Gen.int (Range.linear 0 4)
  srts <- forM [0 .. srtCount - 1] $ \index ->
    CSrt index
      <$> Gen.list (Range.linear 0 3) (Gen.int (Range.constant 0 (staticRootedCount - 1)))
      <*> Gen.list (Range.linear 0 3) (Gen.int (Range.constant 0 (srtCount - 1)))
  staticSrts <- forM [0 .. staticRootedCount - 1] $ \slot -> CSSrt slot <$> genSrt srtCount
  current <- CCurrentSrt <$> genSrt srtCount
  let setup = [CMachine globals roots (8 * spaceWords)] <> srts <> staticSrts <> [current] <> [CMvars mvarCount | mvarCount > 0]
  epochCount <- Gen.int (Range.constant 1 (pEpochMax profile))
  epochs <- genEpochs config profile (applyCommands setup emptyModel) epochCount
  pure (setup <> epochs)

genSrt :: Int -> Gen (Maybe Int)
genSrt 0 = pure Nothing
genSrt count = weighted [(1, pure Nothing), (2, Just <$> Gen.int (Range.constant 0 (count - 1)))]

genEpochs :: Config -> Profile -> Model -> Int -> Gen [Command]
genEpochs _ _ _ 0 = pure []
genEpochs config profile model count = do
  (commands, next) <- genEpoch config profile model
  rest <- genEpochs config profile next (count - 1)
  pure (commands <> rest)

data Shape = ShapeObject Kind [Bool] | ShapeArray Int

shapeWords :: Shape -> Int
shapeWords (ShapeObject kind pointers)
  | kind == KThunk = 1 + max 1 (length pointers)
  | otherwise = 1 + length pointers
shapeWords (ShapeArray count) = 2 + count

genShape :: Profile -> Gen Shape
genShape profile =
  weighted
    [ (3, object KNode),
      (1, object KClosure),
      (pThunkWeight profile + 1, object KThunk),
      (1, object KPartial),
      (pArrayWeight profile, ShapeArray <$> Gen.int (Range.linear 0 (pArrayMax profile)))
    ]
  where
    object kind = do
      count <- Gen.int (Range.linear 0 (pFieldMax profile))
      ShapeObject kind <$> replicateM count (percent (pPointerPercent profile))

-- | The objects and static slots that later commands may name. Heap objects
-- are the resolved live set at the start of the epoch plus the new block.
-- Static slots exclude stale slots, because the collector reads through the
-- invalid target of any static object that becomes live again.
data Pool = Pool
  { poolHeap :: [Id],
    poolStatics :: [Int],
    poolSrts :: [Int]
  }

genEpoch :: Config -> Profile -> Model -> Gen ([Command], Model)
genEpoch config profile start = do
  -- The generator does not know whether the reservation collects, so its
  -- model assumes that it does. A collection that did not happen keeps more
  -- objects and static slots valid, so the assumption is conservative.
  let collected = collectModel config start
      live = liveness config collected
      stale = taintedStatics collected
      usableSrts = [index | index <- Map.keys (mSrts collected), Set.null (Set.intersection stale (srtClosure collected index))]
  blockCount <- Gen.int (Range.constant 0 (pBlockMax profile))
  let identities = [mNextId collected .. mNextId collected + blockCount - 1]
  shapes <- replicateM blockCount (genShape profile)
  srts <- replicateM blockCount (elementOr Nothing (Nothing : map Just usableSrts))
  let newCommands = zipWith3 newCommand identities shapes srts
      blockWords = sum (map shapeWords shapes)
      pool = Pool (Set.toList (liveHeap live) <> identities) [slot | slot <- [0 .. staticCount - 1], not (Set.member slot stale)] usableSrts
  fill <- do
    wanted <- percent (pFillPercent profile)
    if wanted
      then do
        keep <- Gen.choice [pure blockWords, pure (blockWords + 1), pure (max 0 (blockWords - 1)), Gen.int (Range.linear 0 64)]
        pure [CFill keep]
      else pure []
  let afterBlock = applyCommands newCommands collected
  initial <- concat <$> forM (zip identities shapes) (genInitial profile pool)
  rooting <-
    if null identities
      then pure []
      else
        concat
          <$> forM
            ([CGlobal index | index <- [0 .. length (mGlobals start) - 1]] <> [CRoot index | index <- [0 .. length (mRoots start) - 1]])
            ( \slot -> do
                wanted <- percent (pRootPercent profile)
                if wanted then (\identity -> [slot (VHeap identity)]) <$> Gen.element identities else pure []
            )
  let afterInitial = applyCommands (initial <> rooting) afterBlock
  opCount <- Gen.int (Range.constant 0 (pOpsMax profile))
  (ops, afterOps) <- genOps config profile pool afterInitial opCount
  collect <- percent (pCollectPercent profile)
  let final = if collect then collectModel config afterOps else afterOps
  pure (fill <> [CReserve blockWords] <> newCommands <> initial <> rooting <> ops <> [CCollect | collect], final)
  where
    newCommand identity (ShapeObject kind pointers) srt = CNew identity kind pointers srt
    newCommand identity (ShapeArray count) srt = CArray identity count srt

-- | The static slots that lead to a stale slot: the stale slots themselves,
-- static nodes with a field that names one, evaluated static thunks whose
-- target names one, and static objects whose reference table names one. The
-- collector reads the invalid target of a stale slot when any of them becomes
-- live, so no script names them again.
taintedStatics :: Model -> Set Int
taintedStatics model = grow (Set.fromList [slot | (slot, SStale) <- zip [0 ..] (mStaticThunks model)])
  where
    grow tainted =
      let next = Set.union tainted (Set.fromList [slot | slot <- [0 .. staticRootedCount - 1], leads tainted slot])
       in if next == tainted then tainted else grow next
    leads tainted slot = any (`Set.member` tainted) (edges slot)
    edges slot
      | slot < staticThunkCount = case mStaticThunks model !! slot of
          SThunk -> srtStatics (mStaticThunkSrts model !! slot)
          SInd (VStatic target) -> [target]
          _ -> []
      | otherwise =
          let node = slot - staticThunkCount
           in [target | VStatic target <- mStaticNodes model !! node] <> srtStatics (mStaticNodeSrts model !! node)
    srtStatics = maybe [] (Set.toList . srtClosure model)

srtClosure :: Model -> Int -> Set Int
srtClosure model = go Set.empty Set.empty . pure
  where
    go _ statics [] = statics
    go seen statics (index : rest)
      | Set.member index seen = go seen statics rest
      | otherwise =
          let (objects, children) = fromMaybe ([], []) (Map.lookup index (mSrts model))
           in go (Set.insert index seen) (Set.union statics (Set.fromList objects)) (children <> rest)

genPointer :: Profile -> Pool -> Gen Value
genPointer profile pool =
  weighted
    [ (max 1 (pNullPercent profile), pure VNull),
      (pStaticPercent profile, VStatic <$> elementOr 0 (poolStatics pool)),
      (if null (poolHeap pool) then 0 else 100, VHeap <$> Gen.element (poolHeap pool))
    ]

-- | A pointer to an object, for slots that must not hold null.
genTarget :: Profile -> Pool -> (Value -> Bool) -> Gen (Maybe Value)
genTarget profile pool allowed = do
  let heapChoices = [VHeap identity | identity <- poolHeap pool, allowed (VHeap identity)]
      staticChoices = [VStatic slot | slot <- poolStatics pool, allowed (VStatic slot)]
  if null heapChoices && null staticChoices
    then pure Nothing
    else
      Just
        <$> weighted
          [ (if null heapChoices then 0 else 100, Gen.element heapChoices),
            (if null staticChoices then 0 else max 1 (pStaticPercent profile), Gen.element staticChoices)
          ]

genWord :: Profile -> Pool -> Gen Value
genWord profile pool = do
  decoy <- percent (pDecoyPercent profile)
  if decoy && not (null (poolHeap pool))
    then VDecoy <$> Gen.element (poolHeap pool)
    else
      VWord
        <$> Gen.choice
          [ pure 0,
            fromIntegral <$> Gen.int (Range.linear 0 1000),
            Gen.word64 Range.constantBounded,
            -- A word that looks like a heap address: aligned and in a typical
            -- allocation range on both supported platforms.
            (\high low -> (high `shiftL` 32) .|. (low `shiftL` 3)) <$> Gen.element [0x1, 0x6, 0x7f, 0x5555] <*> Gen.word64 (Range.constant 0 0x1fffffff)
          ]

genInitial :: Profile -> Pool -> (Id, Shape) -> Gen [Command]
genInitial profile pool (identity, shape) = case shape of
  ShapeObject _ pointers -> concat <$> forM (zip [0 ..] pointers) field
  ShapeArray count -> concat <$> forM [0 .. count - 1] element
  where
    field (index, True) = element index
    field (index, False) = do
      value <- genWord profile pool
      pure [CSet identity index value | value /= VWord 0]
    element index = do
      value <- genPointer profile pool
      pure [CSet identity index value | value /= VNull]

genOps :: Config -> Profile -> Pool -> Model -> Int -> Gen ([Command], Model)
genOps _ _ _ model 0 = pure ([], model)
genOps config profile pool model count = do
  commands <- genOp profile pool model
  let next = applyCommands commands model
  (rest, final) <- genOps config profile pool next (count - 1)
  pure (commands <> rest, final)

-- | One change to the heap or the roots. Only mutable objects change:
-- arrays, thunks, static thunks, static nodes, and root slots.
genOp :: Profile -> Pool -> Model -> Gen [Command]
genOp profile pool model = do
  let choices = catMaybes options
  if null choices then pure [] else Gen.choice choices
  where
    object identity = Map.lookup identity (mHeap model)
    plainThunks = [identity | identity <- poolHeap pool, Just (Object KThunk _ _ _ False) <- [object identity]]
    blackholed = [identity | identity <- poolHeap pool, Just (Object _ _ _ _ True) <- [object identity]]
    arrays = [(identity, length elements) | identity <- poolHeap pool, Just (Array elements _) <- [object identity], not (null elements)]
    staticThunks = [slot | (slot, SThunk) <- zip [0 ..] (mStaticThunks model), slot `elem` poolStatics pool]
    staticNodes = [slot | slot <- [staticThunkCount .. staticRootedCount - 1], slot `elem` poolStatics pool]
    -- An update target must not lead back to the thunk itself.
    notSelf identity value = resolve model value /= VHeap identity
    withTarget identity build = do
      target <- genTarget profile pool (notSelf identity)
      pure (maybe [] (pure . build) target)
    nonEmpty list gen = if null list then Nothing else Just gen
    options =
      [ nonEmpty plainThunks $ do
          identity <- Gen.element plainThunks
          withTarget identity (CUpdate identity),
        nonEmpty arrays $ do
          (identity, count) <- Gen.element arrays
          index <- Gen.int (Range.constant 0 (count - 1))
          value <- genPointer profile pool
          pure [CSet identity index value],
        nonEmpty plainThunks $ do
          identity <- Gen.element plainThunks
          pure [CBlackhole identity],
        nonEmpty blackholed $ do
          identity <- Gen.element blackholed
          withTarget identity (CUnblackhole identity),
        nonEmpty staticThunks $ do
          slot <- Gen.element staticThunks
          target <- genTarget profile pool (const True)
          pure (maybe [] (pure . CSUpdate slot) target),
        nonEmpty staticNodes $ do
          slot <- Gen.element staticNodes
          index <- Gen.int (Range.constant 0 (staticNodeFields - 1))
          value <- weighted [(1, pure VNull), (3, VStatic <$> elementOr 0 (poolStatics pool))]
          pure [CSSet slot index value],
        nonEmpty (mGlobals model) $ do
          index <- Gen.int (Range.constant 0 (length (mGlobals model) - 1))
          value <- genPointer profile pool
          pure [CGlobal index value],
        nonEmpty (mRoots model) $ do
          index <- Gen.int (Range.constant 0 (length (mRoots model) - 1))
          value <- genPointer profile pool
          pure [CRoot index value],
        Just $ do
          target <- genTarget profile pool (const True)
          pure (maybe [] (pure . CStable) target),
        nonEmpty (mMvars model) $ do
          index <- Gen.int (Range.constant 0 (length (mMvars model) - 1))
          value <- genPointer profile pool
          Gen.element [[CMvarPut index value], [CMvarTake index]],
        Just $ do
          slot <- Gen.element [SlotFunction, SlotContinuation, SlotValue]
          value <- genPointer profile pool
          pure [CThread slot value],
        Just $ do
          srt <- elementOr Nothing (Nothing : map Just (poolSrts pool))
          pure [CCurrentSrt srt]
      ]

-- * Driver

-- | One driver process per property. The process stays alive across cases
-- and restarts after a crash.
data Driver = Driver
  { driverExecutable :: FilePath,
    driverArguments :: [String],
    driverProcess :: MVar (Maybe Process)
  }

data Process = Process
  { processInput :: Handle,
    processOutput :: Handle,
    processError :: Handle,
    processHandle :: ProcessHandle
  }

-- | Compile the driver against the semispace runtime. Sanitizers are used
-- when the C compiler supports them and the sanitized driver runs here, so
-- the runtime archive is instrumented for this test rather than taken from
-- the store.
compileDriver :: IO (FilePath, FilePath)
compileDriver = do
  root <- lookupEnv "AIHC_TEST_ROOT" >>= maybe (throwIO (userError "AIHC_TEST_ROOT is not set")) pure
  temporary <- getCanonicalTemporaryDirectory
  directory <- createTempDirectory temporary "aihc-gc-fuzz"
  let source = root </> "bin" </> "aihc" </> "compiler" </> "native" </> "test" </> "gc-fuzz" </> "aihc_gc_fuzz.c"
      executable = directory </> "aihc-gc-fuzz"
      base = ["-std=c11", "-O1", "-g", "-Wall", "-Wextra", "-Werror"]
      -- The instrumented and the plain runtime differ in their C arguments,
      -- so each attempt gets its own cached archive.
      buildAndLink extra = do
        attempt <- tryIOError $ do
          build <- cachedRuntimeArchive Llvm RuntimeGcSemispace (extra <> base)
          -- Link with the driver that built the archive, so the sanitizer
          -- runtime of the driver matches the instrumented runtime objects.
          (compiler, _targetArguments) <- backendCompiler Llvm
          let arguments =
                extra
                  <> base
                  <> concatMap (\include -> ["-I", include]) (runtimeBuildIncludeDirectories build)
                  <> [source, runtimeBuildArchive build, "-lm", "-o", executable]
          readProcessWithExitCode compiler arguments ""
        pure $ case attempt of
          Left err -> Left (show err)
          Right (ExitSuccess, _, _) -> Right ()
          Right (ExitFailure _, _, message) -> Left message
      compilePlain =
        buildAndLink []
          >>= either (\message -> throwIO (userError ("cannot compile the collector fuzz driver:\n" <> message))) pure
  sanitized <- buildAndLink ["-fsanitize=address,undefined", "-fno-sanitize-recover=all"]
  case sanitized of
    Left _ -> compilePlain
    Right () -> do
      usable <- driverAnswers executable
      unless usable compilePlain
  pure (directory, executable)

-- | Whether a freshly compiled driver starts and answers a trivial script.
--
-- Compiling with the sanitizers is not enough to know that they work here.
-- Inside the Nix sandbox on macOS the AddressSanitizer runtime never
-- finishes reserving its shadow memory: it stops in
-- @FindDynamicShadowStart@, so every sanitized binary hangs before @main@,
-- down to a hello world. Without this check the driver answers nothing and
-- each script waits out the time limit in 'runScript' instead. Ask the
-- driver a question it can answer immediately, and fall back to the plain
-- build when it cannot.
driverAnswers :: FilePath -> IO Bool
driverAnswers executable = do
  outcome <- timeout (10 * 1000000) (try (readProcessWithExitCode executable [] "end\n"))
  pure $ case outcome :: Maybe (Either SomeException (ExitCode, String, String)) of
    Just (Right (ExitSuccess, output, _)) -> "done" `elem` lines output
    _ -> False

newDriver :: IO (FilePath, FilePath) -> Config -> IO Driver
newDriver getBuild config = do
  (_, executable) <- getBuild
  Driver executable (cfgArgs config) <$> newMVar Nothing

stopDriver :: Driver -> IO ()
stopDriver driver = modifyMVar (driverProcess driver) $ \state -> do
  mapM_ killProcess state
  pure (Nothing, ())

startProcess :: Driver -> IO Process
startProcess driver = do
  (Just input, Just output, Just errors, handle) <-
    createProcess (proc (driverExecutable driver) (driverArguments driver)) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering input (BlockBuffering Nothing)
  pure (Process input output errors handle)

killProcess :: Process -> IO String
killProcess process = do
  terminateProcess (processHandle process)
  _ <- try (hClose (processInput process)) :: IO (Either IOException ())
  _ <- waitForProcess (processHandle process)
  errors <- try (hGetContents (processError process) >>= \text -> length text `seq` pure text) :: IO (Either IOException String)
  pure (fromRight "" errors)

-- | Run one script and return the driver's report lines.
runScript :: Driver -> String -> IO (Either String [String])
runScript driver script = modifyMVar (driverProcess driver) $ \state -> do
  process <- maybe (startProcess driver) pure state
  outcome <- try (exchange process) :: IO (Either SomeException (Maybe (Either String [String])))
  case outcome of
    Right (Just (Right output)) -> pure (Just process, Right output)
    Right (Just (Left message)) -> failed process message
    Right Nothing -> failed process "the driver did not answer within the time limit"
    Left exception -> failed process ("the driver stopped: " <> show exception)
  where
    exchange process = timeout (60 * 1000000) $ do
      hPutStr (processInput process) script
      hPutStr (processInput process) "end\n"
      hFlush (processInput process)
      collectLines (processOutput process) []
    collectLines output acc = do
      line <- hGetLine output
      case words line of
        ["done"] -> pure (Right (reverse acc))
        ("fail" : message) -> pure (Left ("the driver rejected the script: " <> unwords message))
        _ -> collectLines output (line : acc)
    failed process message = do
      errors <- killProcess process
      let detail = if all isSpace errors then "" else "\ndriver stderr:\n" <> errors
      pure (Nothing, Left (message <> detail))
