-- | Quick Look instantiation for applications.
--
-- An application @f a1 .. an@ instantiates the quantifiers of @f@ with
-- fresh /instantiation variables/. Before any argument is checked, the
-- arguments whose types are cheap to know are looked at, and what they
-- reveal about the instantiation variables is recorded. An instantiation
-- variable may thereby stand for a polytype, so an argument that expects
-- one, such as the right operand of @runST $ do ...@, is then checked
-- against that polytype instead of being inferred.
--
-- This follows "A quick look at impredicativity" (Serrano, Hage, Peyton
-- Jones, Vytiniotis, ICFP 2020), which is how GHC types @$@ without a
-- special rule. Here the quick look is on for every application.
module Aihc.Tc.QuickLook
  ( quickLookUnify,
  )
where

import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (unless)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map

-- | Record what an argument type reveals about the instantiation
-- variables of an application.
--
-- This is a one-way unifier. It binds instantiation variables only, and
-- it may bind them to polytypes. It never fails: any part of the two
-- types it cannot match is left to the ordinary wanted equality that the
-- caller emits afterwards. An ordinary meta-variable is never bound here,
-- so a polytype cannot leak into one.
quickLookUnify :: IntSet -> TcType -> TcType -> TcM ()
quickLookUnify instantiationVariables = go
  where
    go expected actual = do
      expected' <- zonkType expected >>= reduceTypeFamilies
      actual' <- zonkType actual >>= reduceTypeFamilies
      case (expected', actual') of
        (TcMetaTv left, TcMetaTv right)
          | left == right -> pure ()
        (TcMetaTv unique, _)
          | isInstantiationVariable unique -> bind unique actual'
        (_, TcMetaTv unique)
          | isInstantiationVariable unique -> bind unique expected'
        (TcMetaTv _, _) -> pure ()
        (_, TcMetaTv _) -> pure ()
        (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody) ->
          go leftBody (applySubst (Map.singleton (tvUnique rightVar) (TcTyVar leftVar)) rightBody)
        (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody)
          | length leftPredicates == length rightPredicates -> go leftBody rightBody
        _ -> do
          children <- decomposeNominalEquality expected' actual'
          mapM_ (mapM_ (uncurry go)) children

    isInstantiationVariable (Unique key) = IntSet.member key instantiationVariables

    -- A binding must respect the occurs check and the declared kind. When
    -- the kinds do not agree the binding is skipped; the ordinary solver
    -- reports the mismatch.
    bind unique ty =
      unless (unique `elem` metaVariables ty) $ do
        declaredKind <- readMetaTvKind unique
        solvedKind <- tcTypeKind ty
        kindResult <- unifyTypes declaredKind solvedKind
        case kindResult of
          Right () -> writeMetaTv unique ty
          Left _ -> pure ()

metaVariables :: TcType -> [Unique]
metaVariables ty =
  case ty of
    TcTyVar {} -> []
    TcMetaTv unique -> [unique]
    TcTyCon _ arguments -> concatMap metaVariables arguments
    TcFunTy argument result -> metaVariables argument <> metaVariables result
    TcForAllTy _ body -> metaVariables body
    TcQualTy predicates body -> concatMap predicateMetaVariables predicates <> metaVariables body
    TcAppTy function argument -> metaVariables function <> metaVariables argument

predicateMetaVariables :: Pred -> [Unique]
predicateMetaVariables predicate =
  case predicate of
    ClassPred _ arguments -> concatMap metaVariables arguments
    EqPred left right -> metaVariables left <> metaVariables right
    IParamPred _ payload -> metaVariables payload
    QuantifiedPred variables antecedents consequent ->
      concatMap (metaVariables . tvKind) variables
        <> concatMap predicateMetaVariables antecedents
        <> predicateMetaVariables consequent
