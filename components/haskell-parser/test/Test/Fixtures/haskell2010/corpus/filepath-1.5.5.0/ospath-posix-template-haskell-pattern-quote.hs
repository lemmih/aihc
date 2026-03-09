{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Repro where

f osp' = [p|((==) osp' -> True)|]
