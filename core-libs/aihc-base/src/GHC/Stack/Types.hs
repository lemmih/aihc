{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module GHC.Stack.Types
  ( SrcLoc (..),
    CallStack (..),
    HasCallStack,
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
  )
where

import GHC.Base (String)
import GHC.Types (Int, List (..))

-- | A source location for one call site.
data SrcLoc = SrcLoc
  { srcLocPackage :: String,
    srcLocModule :: String,
    srcLocFile :: String,
    srcLocStartLine :: Int,
    srcLocStartCol :: Int,
    srcLocEndLine :: Int,
    srcLocEndCol :: Int
  }

-- | A call stack with an optional freeze marker.
data CallStack
  = EmptyCallStack
  | PushCallStack String SrcLoc CallStack
  | FreezeCallStack CallStack

-- | A function with this constraint gets the call stack of its call site.
type HasCallStack = (?callStack :: CallStack)

-- | Make an empty call stack.
emptyCallStack :: CallStack
emptyCallStack = EmptyCallStack

-- | Prevent subsequent pushes to a call stack.
freezeCallStack :: CallStack -> CallStack
freezeCallStack stack@(FreezeCallStack _) = stack
freezeCallStack stack = FreezeCallStack stack

-- | Make a call stack from entries in most-recent-first order.
fromCallSiteList :: [(String, SrcLoc)] -> CallStack
fromCallSiteList [] = EmptyCallStack
fromCallSiteList ((name, location) : entries) =
  PushCallStack name location (fromCallSiteList entries)

-- | Get call-stack entries in most-recent-first order.
getCallStack :: CallStack -> [(String, SrcLoc)]
getCallStack EmptyCallStack = []
getCallStack (PushCallStack name location stack) =
  (name, location) : getCallStack stack
getCallStack (FreezeCallStack stack) = getCallStack stack

-- | Add one entry unless the call stack is frozen.
pushCallStack :: (String, SrcLoc) -> CallStack -> CallStack
pushCallStack _ stack@(FreezeCallStack _) = stack
pushCallStack (name, location) stack = PushCallStack name location stack
