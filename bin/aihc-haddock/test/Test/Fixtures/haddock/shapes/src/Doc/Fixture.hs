-- | Module documentation for "Doc.Fixture".
--
-- It mentions 'Shape' and 'area'.
module Doc.Fixture
  ( -- * Shapes
    Shape (..),
    area,
    -- ** Helpers
    Named (..),
    -- $extra
    scale,
    (<+>),
  )
where

-- $extra
-- A named chunk about /helpers/ and @code@.

-- | A shape.
data Shape
  = -- | A circle with a radius.
    Circle Double
  | Rect
      { width :: Double
        -- ^ the width
      , height :: Double
        -- ^ the height
      }
  -- ^ A rectangle.
  deriving (Eq, Show)

-- | Compute the area.
--
-- >>> area (Circle 1)
-- 3.14
--
-- @
-- area (Rect 2 3) == 6
-- @
area ::
  -- | the shape
  Shape ->
  -- | its area
  Double
area (Circle r) = 3.14 * r * r
area (Rect w h) = w * h

-- | Things with names.
class Named a where
  -- | The name.
  name :: a -> String
  name _ = "anonymous"

  -- | Rename.
  rename :: String -> a -> a

instance Named Shape where
  rename _ s = s

-- | Scale a shape.
scale :: Double -- ^ factor
      -> Shape -- ^ input
      -> Shape
scale k (Circle r) = Circle (k * r)
scale k (Rect w h) = Rect (k * w) (k * h)

infixl 6 <+>

-- | Combine two areas.
(<+>) :: Shape -> Shape -> Double
a <+> b = area a + area b
