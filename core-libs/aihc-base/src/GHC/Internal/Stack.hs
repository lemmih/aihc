{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Call-stack rendering. GHC spreads these over @GHC.Stack@ and
-- @GHC.Exception@; they live here so that @GHC.Err@ can render a call
-- stack without importing either.
module GHC.Internal.Stack
  ( popCallStack,
    prettyCallStack,
    prettyCallStackLines,
    prettySrcLoc,
    appendCallStack,
  )
where

import GHC.Base (String)
import GHC.Prim (Word#, chr#, eqWord#, int2Word#, minusWord#, quotRemWord#, word2Int#, (+#), (<#))
import GHC.Stack.Types (CallStack (..), SrcLoc (..), getCallStack)
import GHC.Types (Char (..), Int (..), List (..))

-- | Remove the most recent entry unless the call stack is frozen.
popCallStack :: CallStack -> CallStack
popCallStack EmptyCallStack = EmptyCallStack
popCallStack (PushCallStack _ _ stack) = stack
popCallStack stack@(FreezeCallStack _) = stack

-- | Render a source location as @file:line:column in package:module@.
prettySrcLoc :: SrcLoc -> String
prettySrcLoc location =
  srcLocFile location
    ++ (':' : showInt (srcLocStartLine location))
    ++ (':' : showInt (srcLocStartCol location))
    ++ " in "
    ++ srcLocPackage location
    ++ (':' : srcLocModule location)

-- | Render a call stack with one line for each entry.
prettyCallStack :: CallStack -> String
prettyCallStack stack = joinLines (prettyCallStackLines stack)

-- | The lines of a rendered call stack. An empty call stack has no lines.
prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines stack =
  case getCallStack stack of
    [] -> []
    entries -> "CallStack (from HasCallStack):" : mapList prettyEntry entries
  where
    prettyEntry (name, location) = "  " ++ name ++ ", called at " ++ prettySrcLoc location

-- | Add a rendered call stack to an error message.
appendCallStack :: String -> CallStack -> String
appendCallStack message stack =
  case prettyCallStackLines stack of
    [] -> message
    entries -> message ++ ('\n' : joinLines entries)

infixr 5 ++

(++) :: [a] -> [a] -> [a]
(++) [] suffix = suffix
(++) (value : values) suffix = value : (values ++ suffix)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList function (value : values) = function value : mapList function values

joinLines :: [String] -> String
joinLines [] = []
joinLines [line] = line
joinLines (line : lines') = line ++ ('\n' : joinLines lines')

showInt :: Int -> String
showInt (I# value) =
  case (<#) value 0# of
    0# -> showUnsignedInt (int2Word# value) []
    _ -> '-' : showUnsignedInt (minusWord# (int2Word# 0#) (int2Word# value)) []

showUnsignedInt :: Word# -> String -> String
showUnsignedInt value suffix =
  case quotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqWord# quotient (int2Word# 0#) of
        1# -> digitChar remainder : suffix
        _ -> showUnsignedInt quotient (digitChar remainder : suffix)

digitChar :: Word# -> Char
digitChar digit = C# (chr# ((+#) (word2Int# digit) 48#))
