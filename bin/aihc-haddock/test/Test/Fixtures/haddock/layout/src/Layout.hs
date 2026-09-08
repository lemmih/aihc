-- | Where does a trailing comment go when layout decides?
module Layout
  ( Config (..),
    Runnable (..),
    build,
    step,
  )
where

-- | Configuration.
data Config = Config Int Bool
-- ^ A column-one comment after a data declaration.

-- | Something that runs.
class Runnable a where
  run :: a -> IO ()
  -- ^ Indented like a method: documents the method.

  halt :: a -> IO ()
-- ^ Column one after the class body: closes the block.

build :: Int -> Config
build n = Config n True
-- ^ Column one after a function binding.

-- | Step.
step :: Config -> Int -> Config
  -- ^ Indented after the result type.
step c _ = c
