-- | Put parallel moves in an order that preserves every source value.
module Aihc.Native.Move (orderMoves) where

import Data.List (partition)
import Data.Maybe (mapMaybe)

-- | The scratch location must not be a destination in the input moves.
-- The destination locations must be distinct.
orderMoves :: (Eq location) => (source -> Maybe location) -> (location -> source) -> location -> [(location, source)] -> [(location, source)]
orderMoves locationOf fromLocation scratch = go . filter (\(destination, source) -> locationOf source /= Just destination)
  where
    go [] = []
    go pending =
      case partitionReady pending of
        ([], blocked@((destination, _) : _)) ->
          (scratch, fromLocation destination) : go (map (redirect destination) blocked)
        (ready, blocked) -> ready <> go blocked
    partitionReady pending =
      let sources = mapMaybe (locationOf . snd) pending
       in partition ((`notElem` sources) . fst) pending
    redirect saved (destination, source)
      | locationOf source == Just saved = (destination, fromLocation scratch)
      | otherwise = (destination, source)
