{-# LANGUAGE OverloadedStrings #-}

-- | Congruence closure with explicit nominal evidence.
module Aihc.Tc.Solve.Congruence
  ( proveGivenEquality,
  )
where

import Aihc.Tc.Evidence
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Applicative ((<|>))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set

type ProofGraph = Map TcType [(TcType, Coercion)]

-- | Every graph edge proves equality between its two vertices.
-- Closure uses only subterms of the givens and the wanted.
-- Each new edge joins two components, so closure terminates.
proveGivenEquality :: [Pred] -> TcType -> TcType -> TcM (Maybe Coercion)
proveGivenEquality predicates left right
  | left == right = pure (Just (Refl left))
  | null equalities = pure Nothing
  | otherwise = do
      arrow <- mkKnownTyCon "GHC.Types" "(->)" 2 (KFun KType (KFun KType KType))
      let graph = List.foldl' addGiven Map.empty equalities
          vertices = Set.toList (Set.unions (map (subterms arrow) (left : right : concat [[a, b] | (a, b, _) <- equalities])))
          pairs = [(a, b) | a : rest <- List.tails vertices, b <- rest]
      pure (findProof graph left right <|> findProof (closeGraph arrow pairs graph) left right)
  where
    equalities = [(a, b, GivenCo predicate) | predicate@(EqPred a b) <- predicates]
    addGiven graph (a, b, proof) = addProof a b proof graph

addProof :: TcType -> TcType -> Coercion -> ProofGraph -> ProofGraph
addProof left right proof =
  Map.insertWith (<>) left [(right, proof)]
    . Map.insertWith (<>) right [(left, Sym proof)]

findProof :: ProofGraph -> TcType -> TcType -> Maybe Coercion
findProof graph source target = go (Set.singleton source) [(source, Refl source)]
  where
    go _ [] = Nothing
    go visited ((current, proof) : rest)
      | current == target = Just proof
      | otherwise =
          let edges = [(next, compose proof edge) | (next, edge) <- Map.findWithDefault [] current graph, next `Set.notMember` visited]
              visited' = Set.union visited (Set.fromList (map fst edges))
           in go visited' (rest <> edges)

    compose (Refl _) proof = proof
    compose proof (Refl _) = proof
    compose first second = Trans first second

closeGraph :: TyCon -> [(TcType, TcType)] -> ProofGraph -> ProofGraph
closeGraph arrow pairs graph =
  case List.foldl' extend (False, graph) pairs of
    (False, _) -> graph
    (True, graph') -> closeGraph arrow pairs graph'
  where
    extend (changed, current) (left, right)
      | isJust (findProof current left right) = (changed, current)
      | otherwise =
          case congruence arrow current left right of
            Nothing -> (changed, current)
            Just proof -> (True, addProof left right proof current)

-- | Congruence permits equality under a family as well as under a data type.
-- This rule does not project equality out of a family application.
congruence :: TyCon -> ProofGraph -> TcType -> TcType -> Maybe Coercion
congruence arrow graph left right =
  case (left, right) of
    (TcFunTy a b, TcFunTy c d) ->
      FunCo <$> findProof graph a c <*> findProof graph b d
    (TcTyCon leftCon leftArgs, TcTyCon rightCon rightArgs)
      | leftCon == rightCon,
        length leftArgs == length rightArgs ->
          TyConAppCo leftCon <$> traverse (uncurry (findProof graph)) (zip leftArgs rightArgs)
    _ -> do
      (leftFunction, leftArgument) <- application arrow left
      (rightFunction, rightArgument) <- application arrow right
      AppCo <$> findProof graph leftFunction rightFunction <*> findProof graph leftArgument rightArgument

application :: TyCon -> TcType -> Maybe (TcType, TcType)
application arrow ty =
  case ty of
    TcAppTy function argument -> Just (function, argument)
    TcTyCon tyCon arguments
      | not (null arguments) -> Just (TcTyCon tyCon (init arguments), last arguments)
    TcFunTy domain range -> Just (TcTyCon arrow [domain], range)
    _ -> Nothing

subterms :: TyCon -> TcType -> Set.Set TcType
subterms arrow ty =
  Set.insert ty $ case application arrow ty of
    Just (function, argument) -> subterms arrow function <> subterms arrow argument
    Nothing -> Set.empty
