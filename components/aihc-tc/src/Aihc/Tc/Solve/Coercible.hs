{-# LANGUAGE OverloadedStrings #-}

-- | Check representation constraints before FC conversion.
module Aihc.Tc.Solve.Coercible (isCoercibleClass, solveCoercible) where

import Aihc.Tc.Env
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (matchTypes, reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.Map.Strict qualified as Map

isCoercibleClass :: TyCon -> Bool
isCoercibleClass constructor =
  tyConModuleName constructor == "Data.Coerce"
    && tyConName constructor == "Coercible"

-- | Use nominal arguments unless a container has a known representation role.
-- Unknown outer types wait for other constraints.
solveCoercible :: TcType -> TcType -> TcM Bool
solveCoercible = go [] False
  where
    go visited nested rawLeft rawRight = do
      left <- zonkType rawLeft >>= reduceTypeFamilies
      right <- zonkType rawRight >>= reduceTypeFamilies
      if left == right
        then pure True
        else
          if length visited >= 100 || (left, right) `elem` visited
            then pure False
            else shapes ((left, right) : visited) nested left right
    shapes visited _ (TcFunTy a b) (TcFunTy c d) = do
      argument <- go visited True a c
      result <- go visited True b d
      pure (argument && result)
    shapes visited _ (TcTyCon left args) (TcTyCon right args')
      | left == right,
        length args == length args' = do
          and
            <$> sequence
              [ do
                  representational <- representationParameter [] left index
                  if representational then go visited True argument argument' else nominal argument argument'
              | (index, (argument, argument')) <- zip [0 ..] (zip args args')
              ]
    shapes visited nested left right = do
      leftRepresentation <- representation left
      rightRepresentation <- representation right
      case (leftRepresentation, rightRepresentation) of
        (Just inner, _) -> go visited nested inner right
        (_, Just inner) -> go visited nested left inner
        _
          | nested -> nominal left right
          | otherwise -> pure False
    nominal left right = do
      result <- unifyTypes left right
      pure $ case result of
        Right () -> True
        _ -> False
    representation (TcTyCon constructor arguments) = do
      info <- lookupDataType constructor
      case info of
        Just dataType
          | dtiFlavor dataType == NewtypeTyCon,
            length arguments == length (dtiTyVars dataType),
            [con] <- dtiConstructors dataType,
            [field] <- dciFields con -> do
              let (package, moduleName') = dciOrigin con
              visible <- isTermVisible (TcTermGlobal package moduleName' (dciName con))
              pure
                ( if visible
                    then Just (applySubst (Map.fromList (zip (map tvUnique (dtiTyVars dataType)) arguments)) (dcfiType field))
                    else Nothing
                )
        _ -> pure Nothing
    representation _ = pure Nothing

-- | A parameter can change representation only through representation positions.
-- Recursive data types use the same parameter check at each occurrence.
representationParameter :: [(TyCon, Int)] -> TyCon -> Int -> TcM Bool
representationParameter visited constructor index
  | (constructor, index) `elem` visited = pure True
  | otherwise = do
      info <- lookupDataType constructor
      case info of
        Just dataType
          | index < length (dtiTyVars dataType),
            not (or (take 1 (drop index (dtiNominalRoles dataType)))) -> do
              let parameter = dtiTyVars dataType !! index
                  expected = TcTyCon constructor (map TcTyVar (dtiTyVars dataType))
              and <$> mapM (checkConstructor parameter expected) (dtiConstructors dataType)
          | otherwise -> pure False
        Nothing -> builtinRepresentation constructor
  where
    next = (constructor, index) : visited
    checkConstructor parameter expected con
      | null (dciTheta con),
        null (dciExTyVars con),
        Just substitution <- matchTypes [dciResTy con] [expected] =
          and <$> mapM (representationPosition next parameter . applySubst substitution . dcfiType) (dciFields con)
      | otherwise = pure False

builtinRepresentation :: TyCon -> TcM Bool
builtinRepresentation constructor
  | (tyConModuleName constructor == "GHC.Types" && tyConName constructor == "[]")
      || (tyConModuleName constructor == "GHC.Tuple" && tyConName constructor == boxedTupleTyConName (tyConArity constructor)) = do
      let arity = tyConArity constructor
      expected <- mkKnownTyCon (tyConModuleName constructor) (tyConName constructor) arity (foldr KFun KType (replicate arity KType))
      pure (constructor == expected)
  | otherwise = pure False

representationPosition :: [(TyCon, Int)] -> TyVarId -> TcType -> TcM Bool
representationPosition visited variable ty = case ty of
  TcTyVar binder -> pure (not (mentions variable (tvKind binder)))
  TcMetaTv _ -> pure False
  TcFunTy argument result -> do
    left <- representationPosition visited variable argument
    right <- representationPosition visited variable result
    pure (left && right)
  TcTyCon constructor arguments ->
    and <$> zipWithM checkArgument [0 ..] arguments
    where
      checkArgument index argument
        | not (mentions variable argument) = pure True
        | otherwise = do
            allowed <- representationParameter visited constructor index
            if allowed then representationPosition visited variable argument else pure False
  TcAppTy function argument
    | mentions variable argument -> pure False
    | otherwise -> representationPosition visited variable function
  TcForAllTy binder body
    | mentions variable (tvKind binder) -> pure False
    | otherwise -> representationPosition visited variable body
  TcQualTy _ _ -> pure False

mentions :: TyVarId -> TcType -> Bool
mentions variable = elem (tvUnique variable) . variables
  where
    variables ty = nub $ case ty of
      TcTyVar binder -> [tvUnique binder] <> variables (tvKind binder)
      TcMetaTv _ -> []
      TcTyCon _ arguments -> concatMap variables arguments
      TcFunTy argument result -> variables argument <> variables result
      TcAppTy function argument -> variables function <> variables argument
      TcForAllTy binder body -> variables (tvKind binder) <> filter (/= tvUnique binder) (variables body)
      TcQualTy predicates body -> concatMap predicateVariables predicates <> variables body
    predicateVariables predicate = case predicate of
      ClassPred _ arguments -> concatMap variables arguments
      EqPred left right -> variables left <> variables right
      IParamPred _ payload -> variables payload
      QuantifiedPred binders antecedents consequent ->
        concatMap (filter (`notElem` map tvUnique binders) . predicateVariables) (consequent : antecedents)
