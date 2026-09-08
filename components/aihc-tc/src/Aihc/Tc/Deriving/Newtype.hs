{-# LANGUAGE OverloadedStrings #-}

-- | Check newtype method casts before FC conversion.
module Aihc.Tc.Deriving.Newtype (checkNewtypeInstance) where

import Aihc.Tc.Annotations
import Aihc.Tc.Deriving.Context (newtypeRepresentation, typeTyVars)
import Aihc.Tc.Env
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Coercible (isRepresentationParameter)
import Aihc.Tc.Solve.Dict (matchTypes)
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

checkNewtypeInstance :: (Text, Text) -> (Text -> [Pred] -> Pred -> TcM EvTerm) -> (ClassInfo -> [TcType] -> Text -> TcM TypeScheme) -> TcDerivingPlan -> ClassInfo -> [Pred] -> TcInstanceAnnotation -> TcM TcInstanceAnnotation
checkNewtypeInstance origin solve methodScheme original info context annotation = do
  let substitution = Map.fromList [(tvUnique old, TcTyVar new) | old <- tcDerivingTyVars original, new <- tcInstanceTyVars annotation, tvName old == tvName new]
      plan = original {tcDerivingHeadTypes = tcInstanceHeadTypes annotation}
  case newtypeRepresentation plan of
    Left message -> reject message >> pure annotation
    Right rawRepresentation -> do
      let representation = applySubst substitution rawRepresentation
          headTypes = init (tcInstanceHeadTypes annotation) <> [representation]
      sourceSchemes <- mapM (methodScheme info headTypes . fst) (ciMethods info)
      headKinds <- mapM tcTypeKind headTypes
      let kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (ciTyVars info)) headKinds)
          classSubstitution = Map.fromList (zip (map tvUnique (ciTyVars info)) headTypes) <> kindSubstitution
          superclassFields = map (applySubst classSubstitution) (ciSuperClassTypes info)
          fieldTypes = superclassFields <> map fieldType sourceSchemes
      methods <- zipWithM (checkMethod plan headTypes) [length superclassFields ..] (map fst (ciMethods info))
      evidence <- if null methods then pure Nothing else Just <$> solve (ciName info) context (ClassPred (ciTyCon info) headTypes)
      case evidence of
        Just term | mentionsSelf term -> reject "newtype deriving requires non-circular representation evidence"
        _ -> pure ()
      dictionaryCast <- case (tcDerivingDataType plan, tcInstanceSuperClasses annotation, evidence) of
        (Just dataType, [], Just _) | null (ciKindTyVars info) -> do
          proof <- newtypeCoercion (tcInstanceAssociatedTypes annotation) dataType representation (last (tcInstanceHeadTypes annotation))
          pure (TyConAppCo (ciTyCon info) headTypes . (map Refl (init headTypes) <>) . (: []) <$> proof)
        _ -> pure Nothing
      pure annotation {tcInstanceNewtype = Just (TcNewtypeInstance headTypes evidence fieldTypes dictionaryCast (catMaybes methods))}
  where
    reject = emitError (tcDerivingSourceSpan original) . OtherError
    fieldType (ForAll variables predicates body) =
      foldr TcForAllTy (if null predicates then body else TcQualTy predicates body) variables
    checkMethod plan headTypes index name = do
      ForAll variables sourcePredicates source <- methodScheme info headTypes name
      ForAll _ targetPredicates target <- methodScheme info (tcInstanceHeadTypes annotation) name
      proof <- case tcDerivingDataType plan of
        Just dataType | sourcePredicates == targetPredicates -> newtypeCoercion (tcInstanceAssociatedTypes annotation) dataType source target
        _ -> pure Nothing
      case proof of
        Nothing -> reject ("newtype deriving cannot prove a safe coercion for method " <> T.unpack name) >> pure Nothing
        Just coercion -> pure (Just (TcNewtypeMethod name index variables targetPredicates coercion))
    mentionsSelf term = case term of
      EvDict dictionaryOrigin name _ arguments -> (dictionaryOrigin == origin && name == tcInstanceDictName annotation) || any mentionsSelf arguments
      EvSuperClass inner _ _ _ _ -> mentionsSelf inner
      EvCast inner _ -> mentionsSelf inner
      EvTypeLam _ inner -> mentionsSelf inner
      EvDictLam _ _ inner -> mentionsSelf inner
      EvTypeApp inner _ -> mentionsSelf inner
      EvDictApp function argument -> mentionsSelf function || mentionsSelf argument
      _ -> False

-- | Coercions lift through representation parameters, as roles permit.
newtypeCoercion :: [TypeFamilyInstanceInfo] -> DataTypeInfo -> TcType -> TcType -> TcM (Maybe Coercion)
newtypeCoercion equations dataType rawSource rawTarget = go (normalize rawSource) (normalize rawTarget)
  where
    go source target
      | source == target = pure (Just (Refl source))
      | TcTyCon constructor arguments <- target,
        constructor == dtiTyCon dataType,
        length arguments == length (dtiTyVars dataType),
        [con] <- dtiConstructors dataType,
        [field] <- dciFields con,
        let substitution = Map.fromList (zip (map tvUnique (dtiTyVars dataType)) arguments),
        normalize (applySubst substitution (dcfiType field)) == source = do
          kinds <- mapM tcTypeKind arguments
          let kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (dtiTyVars dataType)) kinds)
              kindVariables = filter (`notElem` dtiTyVars dataType) (nub (concatMap (typeTyVars . tvKind) (dtiTyVars dataType)))
              kindArguments = map (applySubst kindSubstitution . TcTyVar) kindVariables
              key = TcAxiomKey (tyConPackageId constructor) (tyConModuleName constructor) ("$ax$" <> dtiName dataType)
          pure (Just (Sym (AxiomInstCo key (kindArguments <> arguments))))
      | TcFunTy sourceArgument sourceResult <- source,
        TcFunTy targetArgument targetResult <- target = do
          argument <- go sourceArgument targetArgument
          result <- go sourceResult targetResult
          pure (FunCo <$> argument <*> result)
      | TcTyCon sourceConstructor sourceArguments <- source,
        TcTyCon targetConstructor targetArguments <- target,
        sourceConstructor == targetConstructor,
        length sourceArguments == length targetArguments = do
          proofs <- sequence <$> zipWithM (argumentProof sourceConstructor) [0 ..] (zip sourceArguments targetArguments)
          case proofs of
            Just coercions -> pure (Just (TyConAppCo sourceConstructor sourceArguments coercions))
            Nothing -> pure (familyProof source target)
      | otherwise = pure (familyProof source target)
    familyProof source target = case [ Sym (AxiomInstCo (typeFamilyAxiomKey equation) arguments)
                                     | equation <- equations,
                                       Just substitution <- [matchTypes [tfiiLeft equation] [target]],
                                       normalize (applySubst substitution (tfiiRight equation)) == source,
                                       let arguments = map (applySubst substitution . TcTyVar) (tfiiTyVars equation)
                                     ] of
      proof : _ -> Just proof
      [] -> Nothing
    -- A representation parameter carries an argument coercion. A nominal one
    -- admits only the argument it already has.
    argumentProof constructor index (sourceArgument, targetArgument) = do
      representational <- isRepresentationParameter constructor index
      if representational
        then go sourceArgument targetArgument
        else pure (if sourceArgument == targetArgument then Just (Refl sourceArgument) else Nothing)
    normalize ty = case ty of
      TcAppTy function argument -> case normalize function of
        TcTyCon constructor arguments -> TcTyCon constructor (arguments <> [normalize argument])
        other -> TcAppTy other (normalize argument)
      TcTyCon constructor arguments -> TcTyCon constructor (map normalize arguments)
      TcFunTy argument result -> TcFunTy (normalize argument) (normalize result)
      _ -> ty
