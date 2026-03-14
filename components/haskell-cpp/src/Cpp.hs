{-# LANGUAGE OverloadedStrings #-}

module Cpp
  ( Config (..),
    defaultConfig,
    Step (..),
    Result (..),
    IncludeRequest (..),
    IncludeKind (..),
    Diagnostic (..),
    Severity (..),
    preprocess,
  )
where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

newtype Config = Config
  { configInputFile :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config {configInputFile = "<input>"}

data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show)

data IncludeRequest = IncludeRequest
  { includePath :: !FilePath,
    includeKind :: !IncludeKind,
    includeFrom :: !FilePath,
    includeLine :: !Int
  }
  deriving (Eq, Show)

data Severity = Warning | Error deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagSeverity :: !Severity,
    diagMessage :: !Text,
    diagFile :: !FilePath,
    diagLine :: !Int
  }
  deriving (Eq, Show)

data Result = Result
  { resultOutput :: !Text,
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show)

data Step
  = Done !Result
  | NeedInclude !IncludeRequest !(Maybe Text -> Step)

preprocess :: Config -> Text -> Step
preprocess cfg input =
  processFile (configInputFile cfg) (T.lines input) 1 [] emptyState finish
  where
    finish st =
      Done
        Result
          { resultOutput = T.intercalate "\n" (reverse (stOutputRev st)),
            resultDiagnostics = reverse (stDiagnosticsRev st)
          }

data EngineState = EngineState
  { stMacros :: !(Map Text Text),
    stOutputRev :: ![Text],
    stDiagnosticsRev :: ![Diagnostic]
  }

emptyState :: EngineState
emptyState =
  EngineState
    { stMacros = M.empty,
      stOutputRev = [],
      stDiagnosticsRev = []
    }

data CondFrame = CondFrame
  { frameOuterActive :: !Bool,
    frameConditionTrue :: !Bool,
    frameInElse :: !Bool,
    frameCurrentActive :: !Bool
  }

currentActive :: [CondFrame] -> Bool
currentActive [] = True
currentActive (f : _) = frameCurrentActive f

type Continuation = EngineState -> Step

processFile :: FilePath -> [Text] -> Int -> [CondFrame] -> EngineState -> Continuation -> Step
processFile _ [] _ _ st k = k st
processFile filePath (line : restLines) lineNo stack st k =
  let active = currentActive stack
      continue st' = processFile filePath restLines (lineNo + 1) stack st' k
      continueWith stack' st' = processFile filePath restLines (lineNo + 1) stack' st' k
      handleDirective directive =
        case directive of
          DirDefine name value ->
            if currentActive stack
              then continue (st {stMacros = M.insert name value (stMacros st)})
              else continue st
          DirUndef name ->
            if currentActive stack
              then continue (st {stMacros = M.delete name (stMacros st)})
              else continue st
          DirInclude kind includeTarget ->
            if currentActive stack
              then
                let req =
                      IncludeRequest
                        { includePath = includeTarget,
                          includeKind = kind,
                          includeFrom = filePath,
                          includeLine = lineNo
                        }
                    nextStep mContent =
                      case mContent of
                        Nothing ->
                          continue
                            (addDiag Error ("missing include: " <> T.pack includeTarget) filePath lineNo st)
                        Just includeText ->
                          let stWithIncludePragma = emitLine (linePragma 1 includeTarget) st
                              resumeParent stAfterInclude =
                                processFile
                                  filePath
                                  restLines
                                  (lineNo + 1)
                                  stack
                                  (emitLine (linePragma (lineNo + 1) filePath) stAfterInclude)
                                  k
                           in processFile includeTarget (T.lines includeText) 1 [] stWithIncludePragma resumeParent
                 in NeedInclude req nextStep
              else continue st
          DirIf expr ->
            let outer = currentActive stack
                cond = evalCondition (stMacros st) expr
                frame = mkFrame outer cond
             in continueWith (frame : stack) st
          DirIfDef name ->
            let outer = currentActive stack
                cond = M.member name (stMacros st)
                frame = mkFrame outer cond
             in continueWith (frame : stack) st
          DirIfNDef name ->
            let outer = currentActive stack
                cond = not (M.member name (stMacros st))
                frame = mkFrame outer cond
             in continueWith (frame : stack) st
          DirElif expr ->
            case stack of
              [] ->
                continue
                  (addDiag Error "#elif without matching #if" filePath lineNo st)
              f : rest ->
                if frameInElse f
                  then
                    continue
                      (addDiag Error "#elif after #else" filePath lineNo st)
                  else
                    let anyTaken = frameConditionTrue f
                        newCond = not anyTaken && evalCondition (stMacros st) expr
                        f' =
                          f
                            { frameConditionTrue = anyTaken || newCond,
                              frameCurrentActive = frameOuterActive f && newCond
                            }
                     in continueWith (f' : rest) st
          DirElse ->
            case stack of
              [] ->
                continue
                  (addDiag Error "#else without matching conditional" filePath lineNo st)
              f : rest ->
                if frameInElse f
                  then
                    continue
                      (addDiag Error "duplicate #else in conditional block" filePath lineNo st)
                  else
                    let newCurrent = frameOuterActive f && not (frameConditionTrue f)
                        f' =
                          f
                            { frameInElse = True,
                              frameCurrentActive = newCurrent
                            }
                     in continueWith (f' : rest) st
          DirEndIf ->
            case stack of
              [] ->
                continue
                  (addDiag Error "#endif without matching conditional" filePath lineNo st)
              _ : rest -> continueWith rest st
          DirWarning msg ->
            if currentActive stack
              then continue (addDiag Warning msg filePath lineNo st)
              else continue st
          DirError msg ->
            if currentActive stack
              then k (addDiag Error msg filePath lineNo st)
              else continue st
          DirUnsupported name ->
            if currentActive stack
              then
                continue
                  (addDiag Warning ("unsupported directive: " <> name) filePath lineNo st)
              else continue st
   in case parseDirective line of
        Nothing ->
          if active
            then continue (emitLine (expandMacros (stMacros st) line) st)
            else continue st
        Just directive ->
          handleDirective directive

mkFrame :: Bool -> Bool -> CondFrame
mkFrame outer cond =
  CondFrame
    { frameOuterActive = outer,
      frameConditionTrue = cond,
      frameInElse = False,
      frameCurrentActive = outer && cond
    }

emitLine :: Text -> EngineState -> EngineState
emitLine line st = st {stOutputRev = line : stOutputRev st}

addDiag :: Severity -> Text -> FilePath -> Int -> EngineState -> EngineState
addDiag sev msg filePath lineNo st =
  st
    { stDiagnosticsRev =
        Diagnostic
          { diagSeverity = sev,
            diagMessage = msg,
            diagFile = filePath,
            diagLine = lineNo
          }
          : stDiagnosticsRev st
    }

linePragma :: Int -> FilePath -> Text
linePragma n path = "#line " <> T.pack (show n) <> " \"" <> T.pack path <> "\""

data Directive
  = DirDefine !Text !Text
  | DirUndef !Text
  | DirInclude !IncludeKind !FilePath
  | DirIf !Text
  | DirIfDef !Text
  | DirIfNDef !Text
  | DirElif !Text
  | DirElse
  | DirEndIf
  | DirWarning !Text
  | DirError !Text
  | DirUnsupported !Text

parseDirective :: Text -> Maybe Directive
parseDirective raw =
  let trimmed = T.stripStart raw
   in if "#" `T.isPrefixOf` trimmed
        then parseDirectiveBody (T.stripStart (T.drop 1 trimmed))
        else Nothing

parseDirectiveBody :: Text -> Maybe Directive
parseDirectiveBody body =
  let (name, rest0) = T.span isIdentChar body
      rest = T.stripStart rest0
   in case name of
        "define" -> parseDefine rest
        "undef" -> DirUndef <$> parseIdentifier rest
        "include" -> parseInclude rest
        "if" -> Just (DirIf rest)
        "ifdef" -> DirIfDef <$> parseIdentifier rest
        "ifndef" -> DirIfNDef <$> parseIdentifier rest
        "isndef" -> DirIfNDef <$> parseIdentifier rest
        "elif" -> Just (DirElif rest)
        "elseif" -> Just (DirElif rest)
        "else" -> Just DirElse
        "endif" -> Just DirEndIf
        "warning" -> Just (DirWarning rest)
        "error" -> Just (DirError rest)
        _ -> Just (DirUnsupported name)

parseDefine :: Text -> Maybe Directive
parseDefine rest = do
  let (name, rest0) = T.span isIdentChar rest
  if T.null name
    then Nothing
    else
      if "(" `T.isPrefixOf` T.stripStart rest0
        then Just (DirUnsupported "define-function-macro")
        else Just (DirDefine name (T.stripStart rest0))

parseIdentifier :: Text -> Maybe Text
parseIdentifier txt =
  let ident = T.takeWhile isIdentChar (T.stripStart txt)
   in if T.null ident then Nothing else Just ident

parseInclude :: Text -> Maybe Directive
parseInclude txt =
  case T.uncons (T.stripStart txt) of
    Just ('"', rest) ->
      let (path, suffix) = T.breakOn "\"" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeLocal (T.unpack path))
    Just ('<', rest) ->
      let (path, suffix) = T.breakOn ">" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeSystem (T.unpack path))
    _ -> Nothing

expandMacros :: Map Text Text -> Text -> Text
expandMacros macros = applyDepth (32 :: Int)
  where
    applyDepth 0 t = t
    applyDepth n t =
      let next = expandOnce macros t
       in if next == t then t else applyDepth (n - 1) next

expandOnce :: Map Text Text -> Text -> Text
expandOnce macros input = T.pack (go (T.unpack input))
  where
    go [] = []
    go (c : cs)
      | isIdentStart c =
          let (ident, rest) = span isIdentChar (c : cs)
              identTxt = T.pack ident
           in T.unpack (M.findWithDefault identTxt identTxt macros) ++ go rest
      | otherwise = c : go cs

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isLetter c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || isAlphaNum c

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

evalCondition :: Map Text Text -> Text -> Bool
evalCondition macros expr = eval expr /= 0
  where
    eval = evalNumeric . replaceRemainingWithZero . expandMacros macros . replaceDefined macros

evalNumeric :: Text -> Integer
evalNumeric input =
  let tokens = tokenize (T.unpack input)
   in case parseExpr tokens of
        (val, _) -> val

data Token = TOp Text | TNum Integer | TIdent Text | TOpenParen | TCloseParen deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (num, rest) = span isDigit (c : cs)
       in TNum (read num) : tokenize rest
  | isIdentStart c =
      let (ident, rest) = span isIdentChar (c : cs)
       in TIdent (T.pack ident) : tokenize rest
  | c == '(' = TOpenParen : tokenize cs
  | c == ')' = TCloseParen : tokenize cs
  | otherwise =
      let (op, rest) = span isOpChar (c : cs)
       in if null op then tokenize (drop 1 cs) else TOp (T.pack op) : tokenize rest

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/%&|!=<>!" :: String)

parseExpr :: [Token] -> (Integer, [Token])
parseExpr = parseOr

binary :: ([Token] -> (Integer, [Token])) -> [Text] -> [Token] -> (Integer, [Token])
binary next ops ts =
  let (v1, ts1) = next ts
   in go v1 ts1
  where
    go v1 (TOp op : ts2) | op `elem` ops =
      let (v2, ts3) = next ts2
       in go (apply op v1 v2) ts3
    go v1 ts2 = (v1, ts2)

    apply "||" a b = if a /= 0 || b /= 0 then 1 else 0
    apply "&&" a b = if a /= 0 && b /= 0 then 1 else 0
    apply "==" a b = if a == b then 1 else 0
    apply "!=" a b = if a /= b then 1 else 0
    apply "<" a b = if a < b then 1 else 0
    apply ">" a b = if a > b then 1 else 0
    apply "<=" a b = if a <= b then 1 else 0
    apply ">=" a b = if a >= b then 1 else 0
    apply "+" a b = a + b
    apply "-" a b = a - b
    apply "*" a b = a * b
    apply "/" a b = if b == 0 then 0 else a `div` b
    apply "%" a b = if b == 0 then 0 else a `mod` b
    apply _ a _ = a

parseOr, parseAnd, parseEq, parseRel, parseAdd, parseMul :: [Token] -> (Integer, [Token])
parseOr = binary parseAnd ["||"]
parseAnd = binary parseEq ["&&"]
parseEq = binary parseRel ["==", "!="]
parseRel = binary parseAdd ["<", ">", "<=", ">="]
parseAdd = binary parseMul ["+", "-"]
parseMul = binary parseUnary ["*", "/", "%"]

parseUnary :: [Token] -> (Integer, [Token])
parseUnary (TOp "!" : ts) = let (v, ts') = parseUnary ts in (if v == 0 then 1 else 0, ts')
parseUnary (TOp "-" : ts) = let (v, ts') = parseUnary ts in (-v, ts')
parseUnary ts = parseAtom ts

parseAtom :: [Token] -> (Integer, [Token])
parseAtom (TNum n : ts) = (n, ts)
parseAtom (TIdent _ : ts) = (0, ts)
parseAtom (TOpenParen : ts) =
  let (v, ts1) = parseExpr ts
   in case ts1 of
        TCloseParen : ts2 -> (v, ts2)
        _ -> (v, ts1)
parseAtom ts = (0, ts)

replaceDefined :: Map Text Text -> Text -> Text
replaceDefined macros = T.pack . go . T.unpack
  where
    go [] = []
    go cs@(c : rest_cs)
      | "defined" `isPrefixOf` cs && not (isIdentChar (safeHead (drop 7 cs))) =
          let rest = dropWhile isSpace (drop 7 cs)
           in case rest of
                '(' : rest1 ->
                  let name = takeWhile isIdentChar (dropWhile isSpace rest1)
                      rest2 = dropWhile isSpace (drop (length name) (dropWhile isSpace rest1))
                   in case rest2 of
                        ')' : rest3 -> (if M.member (T.pack name) macros then " 1 " else " 0 ") ++ go rest3
                        _ -> " 0 " ++ go rest2
                _ ->
                  let name = takeWhile isIdentChar rest
                   in if null name
                        then " 0 " ++ go rest
                        else (if M.member (T.pack name) macros then " 1 " else " 0 ") ++ go (drop (length name) rest)
      | otherwise = c : go rest_cs
    safeHead [] = ' '
    safeHead (x : _) = x

replaceRemainingWithZero :: Text -> Text
replaceRemainingWithZero = T.pack . go . T.unpack
  where
    go [] = []
    go (c : cs)
      | isIdentStart c =
          let (_, rest) = span isIdentChar (c : cs)
           in " 0 " ++ go rest
      | otherwise = c : go cs
