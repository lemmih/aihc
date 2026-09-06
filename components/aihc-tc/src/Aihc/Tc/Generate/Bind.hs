{-# LANGUAGE OverloadedStrings #-}

-- | Shared value-binding helpers for expression-local declarations.
module Aihc.Tc.Generate.Bind
  ( InferExpr,
    freeVarsDecl,
    freeVarsMatch,
    inferLocalDecls,
    inferRhsWithLocals,
    inferGuardedRhss,
    boolTyCon,
    collectRawSigs,
    sigToScheme,
    skolemize,
    schemeToType,
    renderBinderName,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    CaseAlt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    Match (..),
    Name (..),
    NameType (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Resolve.Generic (everything, everywhereM)
import Aihc.Tc.Annotations (pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (TyConInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Generalize (environmentMetaVars, generalizeGroupAndCommitIgnoring, predMetaVars)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Kind (explicitForallNames, scopedSigTyVars, sigToScheme)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (SolveResult (..), solveConstraints)
import Aihc.Tc.Solve.Dict (DictResult (..), solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.InertSet (InertSet (..))
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (foldM, forM_)
import Data.Data (Data)
import Data.Graph qualified as Graph
import Data.List (mapAccumL, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable (cast)

type InferExpr = Expr -> TcM (Expr, TcType, [Ct])

-- | Infer local declarations, then infer a body under the resulting binders.
inferLocalDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDecls inferExpr decls body
  | not (null decls) && all isImplicitParamDecl decls = inferImplicitParamDecls inferExpr decls body
inferLocalDecls inferExpr decls body = do
  components <- localDeclComponents decls
  case components of
    [_] -> inferLocalDeclGroup inferExpr decls body
    _ -> do
      (annotated, result, ty, cts) <- inferComponents components
      pure (Map.elems (Map.fromList annotated), result, ty, cts)
  where
    -- A component sees the generalized binders of the components it
    -- depends on, like a nested let.
    inferComponents [] = do
      (result, ty, cts) <- body
      pure ([], result, ty, cts)
    inferComponents ((indices, componentDecls) : rest) = do
      (componentDecls', (restAnnotated, result), ty, cts) <-
        inferLocalDeclGroup inferExpr componentDecls $ do
          (restAnnotated, result, restTy, restCts) <- inferComponents rest
          pure ((restAnnotated, result), restTy, restCts)
      pure (zip indices componentDecls' <> restAnnotated, result, ty, cts)

-- | Split the declarations of a local group into its strongly connected
-- components, in dependency order. A binding is generalized before the
-- bindings that use it, so a use gets a fresh instance. The equations of
-- one binder stay together. A signature joins the component of each binder
-- it names, so the result lists it more than once.
localDeclComponents :: [Decl] -> TcM [([Int], [Decl])]
localDeclComponents decls = do
  let indexed = zip [0 :: Int ..] decls
  infos <- mapM (\(index, decl) -> (index,decl,,) <$> declaredBinderKeys decl <*> declTermReferences decl) indexed
  let owners = Map.fromListWith min [(key, index) | (index, _, keys, _) <- infos, key <- keys]
      nodeOf (index, _, keys, _) = maybe index (\key -> Map.findWithDefault index key owners) (listToMaybe keys)
      valueNodes = Map.fromListWith (flip (<>)) [(nodeOf info, [index]) | info@(index, _, keys, _) <- infos, not (null keys)]
      nodeDeps = Map.fromListWith (<>) [(nodeOf info, mapMaybe (`Map.lookup` owners) (Set.toList deps)) | info@(_, _, keys, deps) <- infos, not (null keys)]
  signatureNodes <- concat <$> mapM signatureOwners infos
  let attached = Map.fromListWith (<>) [(node, [index]) | (index, key) <- signatureNodes, Just node <- [Map.lookup key owners]]
      graph = [(node, node, Set.toList (Set.fromList (Map.findWithDefault [] node nodeDeps))) | node <- Map.keys valueNodes]
      componentIndices component =
        let nodes = Graph.flattenSCC component
            indices = Set.toList (Set.fromList (concat [Map.findWithDefault [] node valueNodes <> Map.findWithDefault [] node attached | node <- nodes]))
         in (indices, map (decls !!) indices)
      attachedIndices = Set.fromList (concat (Map.elems attached))
      -- A declaration without binders that no component holds, such as a
      -- signature for a binder outside the group, goes last.
      loose = [index | (index, _, keys, _) <- infos, null keys, not (Set.member index attachedIndices)]
      components = map componentIndices (Graph.stronglyConnComp graph)
  pure (if null loose then components else components <> [(loose, map (decls !!) loose)])
  where
    signatureOwners (index, decl, _, _) =
      case peelDeclAnn decl of
        DeclTypeSig names _ -> do
          keys <- mapM resolvedUnqualifiedTermKey names
          pure [(index, key) | key <- keys]
        _ -> pure []

-- | Annotate each occurrence of a generalized binder inside its own group
-- with the type arguments and the given evidence of its scheme. Inside the
-- group the binder was a monomorphic placeholder, so the occurrence has no
-- instantiation. The desugarer then applies the quantified variables and
-- the dictionary parameters, like a top-level recursive function does.
annotateRecursiveOccurrences :: [(UnqualifiedName, TcBinder)] -> [Decl] -> TcM [Decl]
annotateRecursiveOccurrences binders decls = do
  schemes <- Map.fromList . catMaybes <$> mapM schemeEntry binders
  if Map.null schemes
    then pure decls
    else mapM (everywhereM (annotateOccurrence schemes)) decls
  where
    schemeEntry (name, binder) =
      case binder of
        TcIdBinder scheme@(ForAll tyVars predicates _) _
          | not (null tyVars) || not (null predicates) -> do
              key <- resolvedLocalTermKey name
              pure (Just (key, scheme))
        _ -> pure Nothing

    annotateOccurrence :: (Data b) => Map TcTermKey TypeScheme -> b -> TcM b
    annotateOccurrence schemes value =
      case cast value of
        Just (EVar name)
          | Just resolution <- listToMaybe (mapMaybe fromAnnotation (nameAnns name)),
            resolutionNamespace (resolution :: ResolutionAnnotation) == ResolutionNamespaceTerm -> do
              key <- resolvedTermKey name
              case Map.lookup key schemes of
                Just (ForAll tyVars predicates body) -> do
                  evidenceVars <- mapM givenEvidence predicates
                  let pending = pendingAnnotation body (map TcTyVar tyVars) evidenceVars []
                      annotated = case mapMaybe fromAnnotation (nameAnns name) of
                        sp : _ -> EAnn (mkAnnotation (sp :: SourceSpan)) (EAnn (mkAnnotation pending) (EVar name))
                        [] -> EAnn (mkAnnotation pending) (EVar name)
                  pure (fromMaybe value (cast annotated))
                Nothing -> pure value
        _ -> pure value

    givenEvidence predicate = do
      evidence <- freshEvVar
      bindEvidence evidence (EvGiven predicate)
      pure evidence

-- | Every term that a declaration refers to, by resolved key. The walk
-- covers every syntax form, and it does not remove the binders of the
-- declaration, so the result is a superset of the free variables. The
-- dependency analysis only looks up the binders of the group in it.
declTermReferences :: Decl -> TcM (Set.Set TcTermKey)
declTermReferences decl =
  Set.fromList <$> mapM resolvedTermKey [name | name <- everything collectName decl, hasTermResolution name]
  where
    collectName :: (Data b) => b -> [Name]
    collectName value = maybeToList (cast value)
    hasTermResolution name =
      case mapMaybe fromAnnotation (nameAnns name) of
        resolution : _ -> resolutionNamespace (resolution :: ResolutionAnnotation) == ResolutionNamespaceTerm
        [] -> False

-- | The binders that a declaration defines.
declaredBinderKeys :: Decl -> TcM [TcTermKey]
declaredBinderKeys decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name _) -> (: []) <$> resolvedUnqualifiedTermKey name
    DeclValue (PatternBind _ pat _) -> Set.toList <$> patternBinderKeys pat
    DeclPatSyn patSyn -> (: []) <$> resolvedUnqualifiedTermKey (patSynDeclName patSyn)
    _ -> pure []

-- | Infer one dependency component of local declarations, then infer a body
-- under the resulting binders.
inferLocalDeclGroup :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDeclGroup inferExpr decls body = do
  let groups = groupValueDecls decls
  binders <- distinctLocalBinders (concatMap groupBinders groups)
  rawSigs <- collectRawSigs decls
  sigs <- traverse sigToScheme rawSigs
  let scopedSigs = Map.intersectionWith (\rawSig (ForAll tyVars _ _) -> scopedSigTyVars (explicitForallNames rawSig) tyVars) rawSigs sigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList [(key, ty) | (_, key, ty) <- placeholders]
  binderSet <- Set.fromList <$> traverse resolvedLocalTermKey binders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders sigs placeholders $ do
    groupResults <- mapM (inferLocalGroup inferExpr sigs scopedSigs placeholderMap) groups
    let bindingCts = concatMap snd groupResults
    if shouldGen
      then do
        solveResult <- solveConstraints bindingCts
        residuals <- partitionLocalResiduals binderSet placeholderMap groups binders solveResult
        polyBinders <- generalizedBinders sigs binderSet placeholderMap residuals binders
        decls' <- annotateLocalBindingDecls polyBinders (concatMap (renderGroup . fst) groupResults) >>= annotateRecursiveOccurrences polyBinders
        withReboundLocalBinders polyBinders $ do
          (bodyResult, bodyTy, bodyCts) <- body
          pure (decls', bodyResult, bodyTy, localResidualOuterCts residuals ++ bodyCts)
      else do
        monoBinders <- traverse (monomorphicBinder sigs placeholderMap) binders
        decls' <- annotateLocalBindingDecls monoBinders (concatMap (renderGroup . fst) groupResults)
        (bodyResult, bodyTy, bodyCts) <- body
        pure (decls', bodyResult, bodyTy, bindingCts ++ bodyCts)

isImplicitParamDecl :: Decl -> Bool
isImplicitParamDecl decl =
  case peelDeclAnn decl of
    DeclImplicitParam {} -> True
    _ -> False

-- | Infer a group of implicit-parameter bindings, then infer the body.
--
-- Each right-hand side sees only the enclosing bindings. The body sees the
-- new bindings. The group solves each wanted implicit parameter of the body
-- that has a bound name. Other wanted constraints of the body float out.
inferImplicitParamDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferImplicitParamDecls inferExpr decls body = do
  bindings <- mapM (inferImplicitParamDecl inferExpr) decls
  (bodyResult, bodyTy, bodyCts) <- body
  let bound = [(name, ty) | (_, name, ty, _) <- bindings]
  remainingCts <- concat <$> mapM (solveBoundImplicitParam bound) bodyCts
  pure
    ( [decl | (decl, _, _, _) <- bindings],
      bodyResult,
      bodyTy,
      concat [cts | (_, _, _, cts) <- bindings] <> remainingCts
    )

inferImplicitParamDecl :: InferExpr -> Decl -> TcM (Decl, Text, TcType, [Ct])
inferImplicitParamDecl inferExpr decl =
  case decl of
    DeclAnn ann inner -> do
      (inner', name, ty, cts) <- inferImplicitParamDecl inferExpr inner
      pure (DeclAnn ann inner', name, ty, cts)
    DeclImplicitParam name expr maybeDecls -> do
      (expr', maybeDecls', ty, cts) <-
        case maybeDecls of
          Nothing -> do
            (expr', ty, cts) <- inferExpr expr
            pure (expr', Nothing, ty, cts)
          Just whereDecls -> do
            (whereDecls', expr', ty, cts) <- inferLocalDecls inferExpr whereDecls (inferExpr expr)
            pure (expr', Just whereDecls', ty, cts)
      let annotated = DeclAnn (mkAnnotation (pendingAnnotation ty [] [] [])) (DeclImplicitParam name expr' maybeDecls')
      pure (annotated, name, ty, cts)
    _ -> abortTc "implicit-parameter group contains another declaration"

-- | Solve one wanted constraint of the body against the new bindings.
--
-- The name of an implicit parameter determines its type, so the wanted type
-- must unify with the bound type.
solveBoundImplicitParam :: [(Text, TcType)] -> Ct -> TcM [Ct]
solveBoundImplicitParam bound ct =
  case ctPred ct of
    IParamPred name ty
      | Just boundTy <- lookup name bound -> do
          bindEvidence (ctEvVar ct) (EvGiven (IParamPred name boundTy))
          ev <- freshEvVar
          pure [mkWantedCt (EqPred ty boundTy) ev (ctOrigin ct) (ctLoc ct)]
    _ -> pure [ct]

distinctLocalBinders :: [UnqualifiedName] -> TcM [UnqualifiedName]
distinctLocalBinders = fmap snd . foldM addBinder (Set.empty, [])
  where
    addBinder (keys, binders) binder = do
      key <- resolvedLocalTermKey binder
      if Set.member key keys
        then pure (keys, binders)
        else pure (Set.insert key keys, binders <> [binder])

annotateLocalBindingDecls :: [(UnqualifiedName, TcBinder)] -> [Decl] -> TcM [Decl]
annotateLocalBindingDecls binders decls = do
  binderTypes <- Map.fromList <$> mapM binderTypeEntry binders
  mapM (annotateLocalBindingDecl binderTypes) decls
  where
    binderTypeEntry (name, binder) = do
      key <- resolvedLocalTermKey name
      pure (key, binderType binder)

annotateLocalBindingDecl :: Map TcTermKey TcType -> Decl -> TcM Decl
annotateLocalBindingDecl binderTypes decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateLocalBindingDecl binderTypes inner
    DeclValue valueDecl ->
      do
        keys <- valueDeclBinderKeys valueDecl
        case keys of
          key : _
            | Just ty <- Map.lookup key binderTypes ->
                pure (DeclAnn (mkAnnotation (pendingAnnotation ty [] [] [])) decl)
          _ -> pure decl
    _ -> pure decl

binderType :: TcBinder -> TcType
binderType (TcIdBinder scheme _) = schemeToType scheme
binderType (TcMonoIdBinder ty) = ty

valueDeclBinderKeys :: ValueDecl -> TcM [TcTermKey]
valueDeclBinderKeys valueDecl =
  case valueDecl of
    FunctionBind name _ -> (: []) <$> resolvedLocalTermKey name
    PatternBind _ pat _ -> patternBinderKeyList pat

monomorphicBinder :: Map TcTermKey TypeScheme -> Map TcTermKey TcType -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
monomorphicBinder sigs placeholders name =
  do
    key <- resolvedLocalTermKey name
    case Map.lookup key sigs of
      Just scheme -> pure (name, TcIdBinder scheme Closed)
      Nothing -> do
        ty <- maybe freshMetaTv zonkType (Map.lookup key placeholders)
        pure (name, TcMonoIdBinder ty)

-- | Infer an RHS, processing attached @where@ declarations first.
inferRhsWithLocals :: InferExpr -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhsWithLocals inferExpr rhs =
  case rhs of
    UnguardedRhs sp expr maybeDecls ->
      case maybeDecls of
        Nothing -> do
          (expr', ty, cts) <- inferExpr expr
          pure (UnguardedRhs sp expr' Nothing, ty, cts)
        Just decls -> do
          (decls', expr', ty, cts) <- inferLocalDecls inferExpr decls (inferExpr expr)
          pure (UnguardedRhs sp expr' (Just decls'), ty, cts)
    GuardedRhss anns guardedRhss maybeDecls ->
      case maybeDecls of
        Nothing -> do
          (guardedRhss', ty, cts) <- inferGuardedRhss inferExpr guardedRhss
          pure (GuardedRhss anns guardedRhss' Nothing, ty, cts)
        Just decls -> do
          (decls', guardedRhss', ty, cts) <- inferLocalDecls inferExpr decls (inferGuardedRhss inferExpr guardedRhss)
          pure (GuardedRhss anns guardedRhss' (Just decls'), ty, cts)

-- | Infer guarded alternatives. Each body has the shared result type.
inferGuardedRhss :: InferExpr -> [GuardedRhs Expr] -> TcM ([GuardedRhs Expr], TcType, [Ct])
inferGuardedRhss inferExpr guardedRhss = do
  resultTy <- freshMetaTv
  results <- mapM (inferGuardedRhs inferExpr resultTy) guardedRhss
  pure (map fst results, resultTy, concatMap snd results)

inferGuardedRhs :: InferExpr -> TcType -> GuardedRhs Expr -> TcM (GuardedRhs Expr, [Ct])
inferGuardedRhs inferExpr resultTy guardedRhs = do
  let sp = sourceSpanFromAnnotations (guardedRhsAnns guardedRhs)
  (qualifiers', body', cts) <-
    inferGuardQualifiers inferExpr sp resultTy (guardedRhsGuards guardedRhs) $ do
      (body', bodyTy, bodyCts) <- inferExpr (guardedRhsBody guardedRhs)
      ev <- freshEvVar
      let bodyCt = mkWantedCt (EqPred bodyTy resultTy) ev (AppOrigin sp) sp
      pure (body', bodyCts ++ [bodyCt])
  pure (guardedRhs {guardedRhsGuards = qualifiers', guardedRhsBody = body'}, cts)

-- | Infer guard qualifiers from left to right. A pattern guard and a let
-- guard bind names for the qualifiers and the body that follow them.
inferGuardQualifiers :: InferExpr -> SourceSpan -> TcType -> [GuardQualifier] -> TcM (a, [Ct]) -> TcM ([GuardQualifier], a, [Ct])
inferGuardQualifiers inferExpr sp resultTy qualifiers rest =
  case qualifiers of
    [] -> do
      (result, cts) <- rest
      pure ([], result, cts)
    GuardAnn ann inner : more -> do
      (qualifiers', result, cts) <- inferGuardQualifiers inferExpr sp resultTy (inner : more) rest
      case qualifiers' of
        inner' : more' -> pure (GuardAnn ann inner' : more', result, cts)
        [] -> pure ([], result, cts)
    GuardExpr condition : more -> do
      (condition', conditionTy, conditionCts) <- inferExpr condition
      boolTy <- boolTyCon
      ev <- freshEvVar
      let conditionCt = mkWantedCt (EqPred conditionTy boolTy) ev (AppOrigin sp) sp
      (more', result, cts) <- inferGuardQualifiers inferExpr sp resultTy more rest
      pure (GuardExpr condition' : more', result, conditionCts ++ [conditionCt] ++ cts)
    GuardPat pat scrutinee : more -> do
      (scrutinee', scrutineeTy, scrutineeCts) <- inferExpr scrutinee
      patCheck <- checkPattern sp pat scrutineeTy
      (more', result, cts) <- withPatternBindings (pcBindings patCheck) (inferGuardQualifiers inferExpr sp resultTy more rest)
      remainingCts <- solvePatternBranch sp patCheck resultTy cts
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      pure (GuardPat pat' scrutinee' : more', result, scrutineeCts ++ remainingCts)
    GuardLet decls : more -> do
      (decls', (more', result), _ty, cts) <-
        inferLocalDecls inferExpr decls $ do
          (more', result, cts) <- inferGuardQualifiers inferExpr sp resultTy more rest
          pure ((more', result), resultTy, cts)
      pure (GuardLet decls' : more', result, cts)

-- | The 'Bool' type that guards and conditions have.
boolTyCon :: TcM TcType
boolTyCon = do
  maybeInfo <- lookupTyCon "Bool"
  case maybeInfo of
    Just info -> pure (TcTyCon (tciTyCon info) [])
    Nothing -> TcTyCon <$> mkKnownTyCon "GHC.Types" "Bool" 0 typeKindType <*> pure []

placeholderFor :: Map TcTermKey TypeScheme -> UnqualifiedName -> TcM (UnqualifiedName, TcTermKey, TcType)
placeholderFor sigs name = do
  key <- resolvedLocalTermKey name
  ty <- maybe freshMetaTv skolemize (Map.lookup key sigs)
  pure (name, key, ty)

withLocalPlaceholders :: Map TcTermKey TypeScheme -> [(UnqualifiedName, TcTermKey, TcType)] -> TcM a -> TcM a
withLocalPlaceholders sigs placeholders =
  withLocalBinders
    [ (name, maybe (TcMonoIdBinder ty) (`TcIdBinder` Closed) (Map.lookup key sigs))
    | (name, key, ty) <- placeholders
    ]

withLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withLocalBinders [] action = action
withLocalBinders ((name, binder) : rest) action =
  extendResolvedTermEnv name binder (withLocalBinders rest action)

withReboundLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withReboundLocalBinders [] action = action
withReboundLocalBinders ((name, binder) : rest) action = do
  key <- resolvedLocalTermKey name
  rebindTermEnv key binder (withReboundLocalBinders rest action)

-- | The binders of a generalized local group. The bindings without a
-- signature that the monomorphism restriction does not restrict are
-- generalized together, over one shared set of type variables.
generalizedBinders :: Map TcTermKey TypeScheme -> Set.Set TcTermKey -> Map TcTermKey TcType -> LocalResiduals -> [UnqualifiedName] -> TcM [(UnqualifiedName, TcBinder)]
generalizedBinders sigs ignored placeholders residuals binders = do
  classified <- traverse classify binders
  schemes <- generalizeGroupAndCommitIgnoring ignored [(ty, preds) | Right (_, ty, preds) <- classified]
  pure (assemble classified schemes)
  where
    classify name = do
      key <- resolvedLocalTermKey name
      case Map.lookup key sigs of
        Just scheme ->
          pure (Left (name, TcIdBinder scheme Closed))
        Nothing ->
          case Map.lookup key placeholders of
            Nothing -> do
              ty <- freshMetaTv
              pure (Left (name, TcMonoIdBinder ty))
            Just ty
              | key `Set.member` localResidualMonomorphic residuals -> do
                  ty' <- zonkType ty
                  pure (Left (name, TcMonoIdBinder ty'))
              | otherwise ->
                  pure (Right (name, ty, Map.findWithDefault [] key (localResidualPreds residuals)))
    assemble classified schemes =
      case (classified, schemes) of
        ([], _) -> []
        (Left fixed : rest, _) -> fixed : assemble rest schemes
        (Right (name, _, _) : rest, scheme : moreSchemes) -> (name, TcIdBinder scheme Closed) : assemble rest moreSchemes
        (Right (name, ty, _) : rest, []) -> (name, TcMonoIdBinder ty) : assemble rest []

-- | Residual constraints of a local binding group after the group solve.
data LocalResiduals = LocalResiduals
  { -- | Predicates that each generalized binder abstracts over.
    localResidualPreds :: Map TcTermKey [Pred],
    -- | Binders that the monomorphism restriction keeps monomorphic.
    localResidualMonomorphic :: Set.Set TcTermKey,
    -- | Constraints that the enclosing scope must solve.
    localResidualOuterCts :: [Ct]
  }

-- | Split the residual constraints of a local binding group.
--
-- A class constraint on a type variable that a function binder generalizes
-- becomes a dictionary parameter of that binder. The monomorphism
-- restriction keeps a pattern binding or a zero-argument binding
-- monomorphic when a constraint mentions its type. All other constraints
-- go to the enclosing scope.
partitionLocalResiduals :: Set.Set TcTermKey -> Map TcTermKey TcType -> [DeclGroup] -> [UnqualifiedName] -> SolveResult -> TcM LocalResiduals
partitionLocalResiduals binderSet placeholders groups binders solveResult = do
  residualCts <- mapM zonkCtPred (srResidual solveResult <> inertDicts (srInerts solveResult))
  envMetaVars <- environmentMetaVars binderSet
  restricted <- restrictedBinderKeys groups
  binderInfos <- traverse (binderMetaInfo placeholders) binders
  let step (preds, monomorphic, outerCts, givens) ct =
        let predicate = ctPred ct
            generalizable = filter (`notElem` envMetaVars) (predMetaVars predicate)
            owners = [key | (key, metas) <- binderInfos, any (`elem` metas) generalizable]
            restrictedOwners = filter (`Set.member` restricted) owners
         in if null generalizable || null owners || not (null restrictedOwners) || not (isClassPred predicate)
              then (preds, Set.union (Set.fromList restrictedOwners) monomorphic, outerCts ++ [ct], givens)
              else (foldr (\key -> Map.insertWith (flip (++)) key [predicate]) preds owners, monomorphic, outerCts, givens ++ [ct])
      (localPreds, monomorphicKeys, outer, givenCts) = foldl step (Map.empty, Set.empty, [], []) residualCts
  forM_ givenCts $ \ct ->
    bindEvidence (ctEvVar ct) (EvGiven (ctPred ct))
  pure
    LocalResiduals
      { localResidualPreds = localPreds,
        localResidualMonomorphic = monomorphicKeys,
        localResidualOuterCts = outer
      }
  where
    zonkCtPred ct = do
      predicate <- zonkPred (ctPred ct)
      pure (ct {ctPred = predicate})
    isClassPred ClassPred {} = True
    isClassPred _ = False
    binderMetaInfo placeholderMap name = do
      key <- resolvedLocalTermKey name
      ty <- maybe (pure Nothing) (fmap Just . zonkType) (Map.lookup key placeholderMap)
      pure (key, maybe [] typeMetaVars ty)

-- | Binders that the monomorphism restriction applies to: pattern bindings
-- and function bindings without arguments.
restrictedBinderKeys :: [DeclGroup] -> TcM (Set.Set TcTermKey)
restrictedBinderKeys groups = Set.fromList . concat <$> mapM restrictedKeys groups
  where
    restrictedKeys group =
      case group of
        MergedFunctionBind name _ (match : _)
          | null (matchPats match) -> (: []) <$> resolvedLocalTermKey name
        MergedFunctionBind {} -> pure []
        SingleDecl decl ->
          case peelDeclAnn decl of
            DeclValue (PatternBind _ pat _) -> patternBinderKeyList pat
            DeclValue (FunctionBind name (match : _))
              | null (matchPats match) -> (: []) <$> resolvedLocalTermKey name
            _ -> pure []

-- | Free meta-variables of a zonked type.
typeMetaVars :: TcType -> [Unique]
typeMetaVars ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcTyVar _ -> []
    TcTyCon _ args -> concatMap typeMetaVars args
    TcFunTy a b -> typeMetaVars a ++ typeMetaVars b
    TcForAllTy _ body -> typeMetaVars body
    TcQualTy ps body -> concatMap predMetaVars ps ++ typeMetaVars body
    TcAppTy f a -> typeMetaVars f ++ typeMetaVars a

-- | The type variables that the signatures of a local group scope over
-- their bindings, by binder.
type ScopedSigs = Map TcTermKey (Map Text (TyVarId, TcType))

inferLocalGroup :: InferExpr -> Map TcTermKey TypeScheme -> ScopedSigs -> Map TcTermKey TcType -> DeclGroup -> TcM (DeclGroup, [Ct])
inferLocalGroup inferExpr sigs scopedSigs placeholders group =
  case group of
    MergedFunctionBind name decls matches -> do
      (matches', _ty, cts) <- inferLocalFunction inferExpr sigs scopedSigs placeholders name matches
      pure (MergedFunctionBind name (replaceFunctionDeclMatches matches' decls) matches', cts)
    SingleDecl decl -> do
      (decl', cts) <- inferLocalSingleDecl inferExpr sigs scopedSigs placeholders decl
      pure (SingleDecl decl', cts)

inferLocalSingleDecl :: InferExpr -> Map TcTermKey TypeScheme -> ScopedSigs -> Map TcTermKey TcType -> Decl -> TcM (Decl, [Ct])
inferLocalSingleDecl inferExpr sigs scopedSigs placeholders decl =
  case decl of
    DeclAnn ann inner -> do
      (inner', cts) <- inferLocalSingleDecl inferExpr sigs scopedSigs placeholders inner
      pure (DeclAnn ann inner', cts)
    DeclValue valueDecl ->
      case valueDecl of
        PatternBind mult pat rhs ->
          case patternBinderName pat of
            Just name -> do
              (rhs', _ty, cts) <- inferLocalPatternBind inferExpr sigs scopedSigs placeholders name rhs
              pure (DeclValue (PatternBind mult pat rhs'), cts)
            Nothing -> do
              (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
              let sourceSpan = NoSourceSpan
              patCheck <- checkPatternsWithGivens sourceSpan [(pat, rhsTy)]
              patternCts <- solvePatternBranch sourceSpan patCheck rhsTy rhsCts
              cts <- foldM (tiePatternPlaceholder placeholders) patternCts (pcBindings patCheck)
              let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
              pure (DeclValue (PatternBind mult pat' rhs'), cts)
        FunctionBind name matches -> do
          (matches', _ty, cts) <- inferLocalFunction inferExpr sigs scopedSigs placeholders name matches
          pure (DeclValue (FunctionBind name matches'), cts)
    _ -> pure (decl, [])

inferLocalFunction :: InferExpr -> Map TcTermKey TypeScheme -> ScopedSigs -> Map TcTermKey TcType -> UnqualifiedName -> [Match] -> TcM ([Match], TcType, [Ct])
inferLocalFunction inferExpr sigs scopedSigs placeholders name matches = do
  key <- resolvedLocalTermKey name
  (matches', ty, cts) <-
    case Map.lookup key sigs of
      Just scheme -> do
        sigTy <- maybe (skolemize scheme) pure (Map.lookup key placeholders)
        let nArgs =
              case matches of
                m : _ -> length (matchPats m)
                [] -> 0
            (argTys, resTy) = splitFunTy sigTy nArgs
        results <-
          withScopedTyVars (Map.findWithDefault Map.empty key scopedSigs) $
            mapM (tcMatchEquation inferExpr argTys resTy) matches
        let matches' = map fst results
            matchCts = concatMap snd results
        residualCts <- solveWithSigGivens scheme matchCts
        pure (matches', sigTy, residualCts)
      Nothing ->
        tcMatches inferExpr matches
  cts' <- tiePlaceholder placeholders key ty cts
  pure (matches', ty, cts')

-- | Solve the constraints of a local binding under the context of its
-- signature. The desugarer turns the context into dictionary parameters of
-- the binding, so a constraint that the context entails is evidence for a
-- given. Equalities go first so that a dictionary constraint sees the
-- solved meta variables. Whatever stays stuck goes to the enclosing scope,
-- like the constraints of a binding without a signature.
solveWithSigGivens :: TypeScheme -> [Ct] -> TcM [Ct]
solveWithSigGivens (ForAll _ predicates _) cts
  | null predicates = pure cts
  | otherwise = do
      let (equalityCts, dictionaryCts) = partition isEqualityCt cts
      residualEqualities <- concat <$> mapM solveEqualityCt equalityCts
      residualDictionaries <- concat <$> mapM solveDictionaryCt dictionaryCts
      pure (residualEqualities <> residualDictionaries)
  where
    isEqualityCt ct =
      case ctPred ct of
        EqPred {} -> True
        _ -> False

    solveEqualityCt ct = do
      result <- solveEquality ct
      pure $ case result of
        EqSolved -> []
        EqStuck stuck -> [stuck]
        EqError err -> [err]

    solveDictionaryCt ct = do
      result <- solveDictWithGivens predicates ct
      pure $ case result of
        DictSolved -> []
        DictStuck stuck -> [stuck]

inferLocalPatternBind :: InferExpr -> Map TcTermKey TypeScheme -> ScopedSigs -> Map TcTermKey TcType -> UnqualifiedName -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferLocalPatternBind inferExpr sigs scopedSigs placeholders name rhs = do
  key <- resolvedLocalTermKey name
  (rhs', rhsTy, rhsCts) <-
    withScopedTyVars (Map.findWithDefault Map.empty key scopedSigs) $
      inferRhsWithLocals inferExpr rhs
  (ty, bindCts) <-
    case Map.lookup key sigs of
      Just scheme -> do
        sigTy <- maybe (skolemize scheme) pure (Map.lookup key placeholders)
        -- The right-hand side must have the signature type.
        ev <- freshEvVar
        let sigCt = mkWantedCt (EqPred sigTy rhsTy) ev (LetOrigin NoSourceSpan) NoSourceSpan
        residualCts <- solveWithSigGivens scheme (sigCt : rhsCts)
        pure (sigTy, residualCts)
      Nothing -> pure (rhsTy, rhsCts)
  cts <- tiePlaceholder placeholders key ty bindCts
  pure (rhs', ty, cts)

tiePlaceholder :: Map TcTermKey TcType -> TcTermKey -> TcType -> [Ct] -> TcM [Ct]
tiePlaceholder placeholders key ty cts =
  case Map.lookup key placeholders of
    Nothing -> pure cts
    Just placeholderTy -> do
      ev <- freshEvVar
      let eqCt = mkWantedCt (EqPred placeholderTy ty) ev (LetOrigin NoSourceSpan) NoSourceSpan
      pure (cts ++ [eqCt])

tiePatternPlaceholder :: Map TcTermKey TcType -> [Ct] -> (UnqualifiedName, TcType) -> TcM [Ct]
tiePatternPlaceholder placeholders cts (name, ty) = do
  key <- resolvedLocalTermKey name
  tiePlaceholder placeholders key ty cts

tcMatches :: InferExpr -> [Match] -> TcM ([Match], TcType, [Ct])
tcMatches _ [] = do
  ty <- freshMetaTv
  pure ([], ty, [])
tcMatches inferExpr matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      (firstMatch, ty0, cts0) <- inferZeroArgMatch inferExpr m0
      restResults <- mapM (unifyMatchRhs inferExpr ty0) (drop 1 matches)
      let restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (firstMatch : restMatches, ty0, cts0 ++ restCts)
    else do
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      results <- mapM (tcMatchEquation inferExpr argTys resTy) matches
      let matches' = map fst results
          allCts = concatMap snd results
      pure (matches', foldr TcFunTy resTy argTys, allCts)

inferZeroArgMatch :: InferExpr -> Match -> TcM (Match, TcType, [Ct])
inferZeroArgMatch inferExpr match = do
  (rhs', ty, cts) <- inferRhsWithLocals inferExpr (matchRhs match)
  pure (match {matchRhs = rhs'}, ty, cts)

tcMatchEquation :: InferExpr -> [TcType] -> TcType -> Match -> TcM (Match, [Ct])
tcMatchEquation inferExpr argTys resTy match = do
  let pats = matchPats match
      matchSpan = sourceSpanFromAnnotations (matchAnns match)
  patCheck <- checkFunctionPatternsWithGivens matchSpan (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithLocals inferExpr (matchRhs match))
  ev <- freshEvVar
  let rhsLocation = orSourceSpan (rhsSourceSpan (matchRhs match)) matchSpan
      pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin rhsLocation) rhsLocation
      bodyWanteds = rhsCts ++ [resCt]
  remainingCts <- solvePatternBranch rhsLocation patCheck resTy bodyWanteds
  pure (match {matchPats = pats', matchRhs = rhs'}, remainingCts)

sourceSpanFromAnnotations :: [Annotation] -> SourceSpan
sourceSpanFromAnnotations annotations =
  case mapMaybe fromAnnotation annotations of
    sourceSpan : _ -> sourceSpan
    [] -> NoSourceSpan

unifyMatchRhs :: InferExpr -> TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs inferExpr expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr (matchRhs match)
  ev <- freshEvVar
  let rhsLocation = orSourceSpan (rhsSourceSpan (matchRhs match)) (sourceSpanFromAnnotations (matchAnns match))
      eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin rhsLocation) rhsLocation
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

rhsSourceSpan :: Rhs body -> SourceSpan
rhsSourceSpan rhs =
  case rhs of
    UnguardedRhs annotations _ _ -> sourceSpanFromAnnotations annotations
    GuardedRhss annotations _ _ -> sourceSpanFromAnnotations annotations

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sourceSpan _ = sourceSpan

shouldGeneralizeLocal :: Set.Set TcTermKey -> [Decl] -> TcM Bool
shouldGeneralizeLocal binderSet decls = do
  monoLocal <- tcMonoLocalBinds
  -- A strict binding is evaluated once, before the body, so it cannot be
  -- polymorphic. GHC does not generalize a group with a strict binding.
  if any isStrictPatternBind decls
    then pure False
    else
      if not monoLocal || any hasPartialTypeSig decls
        then pure True
        else do
          freeVars <- freeVarsDecls decls
          let externalVars = Set.toList (Set.difference freeVars binderSet)
          allM isClosedVar externalVars

-- | Whether a declaration is a pattern binding with a bang on its pattern.
isStrictPatternBind :: Decl -> Bool
isStrictPatternBind decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat _) -> patternIsStrict pat
    _ -> False
  where
    patternIsStrict pat =
      case pat of
        PAnn _ inner -> patternIsStrict inner
        PParen inner -> patternIsStrict inner
        PStrict _ -> True
        _ -> False

isClosedVar :: TcTermKey -> TcM Bool
isClosedVar key = do
  binder <- lookupTermKey key
  pure $
    case binder of
      Just (TcIdBinder _ Closed) -> True
      _ -> False

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM p = foldM step True
  where
    step False _ = pure False
    step True x = p x

data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind UnqualifiedName [Decl] [Match]

renderGroup :: DeclGroup -> [Decl]
renderGroup group =
  case group of
    SingleDecl decl -> [decl]
    MergedFunctionBind _ decls _ -> decls

groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) =
  case extractFunctionBind d of
    Just (name, matches) ->
      let (sameNameDecls, rest) = span (hasSameName name) ds
          groupDecls = d : sameNameDecls
          allMatches = matches ++ concatMap (maybe [] snd . extractFunctionBind) sameNameDecls
       in MergedFunctionBind name groupDecls allMatches : groupValueDecls rest
    Nothing -> SingleDecl d : groupValueDecls ds

groupBinders :: DeclGroup -> [UnqualifiedName]
groupBinders group =
  case group of
    MergedFunctionBind name _ _ -> [name]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []

extractFunctionBind :: Decl -> Maybe (UnqualifiedName, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> Just (name, matches)
    _ -> Nothing

hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name decl =
  case extractFunctionBind decl of
    Just (declName, _) -> unqualifiedNameText declName == unqualifiedNameText name
    Nothing -> False

replaceFunctionDeclMatches :: [Match] -> [Decl] -> [Decl]
replaceFunctionDeclMatches matches decls =
  snd (mapAccumL replace matches decls)
  where
    replace remaining decl =
      let count = functionDeclMatchCount decl
          (here, rest) = splitAt count remaining
       in (rest, replaceDeclFunctionMatches here decl)

functionDeclMatchCount :: Decl -> Int
functionDeclMatchCount decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind _ matches) -> length matches
    _ -> 0

replaceDeclFunctionMatches :: [Match] -> Decl -> Decl
replaceDeclFunctionMatches matches decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replaceDeclFunctionMatches matches inner)
    DeclValue (FunctionBind name _) -> DeclValue (FunctionBind name matches)
    _ -> decl

collectRawSigs :: [Decl] -> TcM (Map TcTermKey Type)
collectRawSigs decls = Map.fromList . concat <$> mapM extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      mapM (fmap (,ty) . resolvedLocalTermKey) names
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = pure []

skolemize :: TypeScheme -> TcM TcType
skolemize (ForAll _ _ body) = pure body

splitFunTy :: TcType -> Int -> ([TcType], TcType)
splitFunTy ty 0 = ([], ty)
splitFunTy (TcFunTy a rest) n =
  let (args, res) = splitFunTy rest (n - 1)
   in (a : args, res)
splitFunTy ty _ = ([], ty)

schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs
schemeToType (ForAll [] preds ty) = TcQualTy preds ty
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs

patternBinderName :: Pattern -> Maybe UnqualifiedName
patternBinderName (PVar n) = Just n
patternBinderName (PParen inner) = patternBinderName inner
patternBinderName (PAnn _ inner) = patternBinderName inner
patternBinderName _ = Nothing

renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

freeVarsDecls :: [Decl] -> TcM (Set.Set TcTermKey)
freeVarsDecls decls =
  Set.unions <$> mapM freeVarsDecl decls

freeVarsDecl :: Decl -> TcM (Set.Set TcTermKey)
freeVarsDecl decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> do
      vars <- Set.unions <$> mapM freeVarsMatch matches
      binder <- resolvedUnqualifiedTermKey name
      pure (Set.delete binder vars)
    DeclValue (PatternBind _ pat rhs) -> do
      vars <- freeVarsRhs rhs
      patVars <- freeVarsPattern pat
      binders <- patternBinderKeys pat
      pure (Set.difference (vars <> patVars) binders)
    DeclImplicitParam _ expr maybeDecls -> freeVarsRhs (UnguardedRhs [] expr maybeDecls)
    DeclTypeSig {} -> pure Set.empty
    DeclPatSyn patSyn -> do
      patVars <- freeVarsPattern (patSynDeclPat patSyn)
      builderVars <-
        case patSynDeclDir patSyn of
          PatSynExplicitBidirectional matches -> Set.unions <$> mapM freeVarsMatch matches
          _ -> pure Set.empty
      binder <- resolvedUnqualifiedTermKey (patSynDeclName patSyn)
      pure (Set.delete binder (patVars <> builderVars))
    _ -> pure Set.empty

freeVarsMatch :: Match -> TcM (Set.Set TcTermKey)
freeVarsMatch match = do
  vars <- freeVarsRhs (matchRhs match)
  patVars <- Set.unions <$> mapM freeVarsPattern (matchPats match)
  binders <- Set.unions <$> mapM patternBinderKeys (matchPats match)
  pure (Set.difference (vars <> patVars) binders)

-- | The term variables that a pattern uses: the view functions and the
-- constructors. A constructor can be a pattern synonym of the same group.
freeVarsPattern :: Pattern -> TcM (Set.Set TcTermKey)
freeVarsPattern pat =
  case pat of
    PAnn ann inner -> do
      innerVars <- freeVarsPattern inner
      insertSyntaxTermKey ann innerVars
    PParen inner -> freeVarsPattern inner
    PAs _ inner -> freeVarsPattern inner
    PStrict inner -> freeVarsPattern inner
    PIrrefutable inner -> freeVarsPattern inner
    PTypeSig inner _ -> freeVarsPattern inner
    PList items -> Set.unions <$> mapM freeVarsPattern items
    PTuple _ items -> Set.unions <$> mapM freeVarsPattern items
    PUnboxedSum _ _ inner -> freeVarsPattern inner
    PCon name _ subPats -> Set.insert <$> resolvedTermKey name <*> (Set.unions <$> mapM freeVarsPattern subPats)
    PBuiltinCon _ _ subPats -> Set.unions <$> mapM freeVarsPattern subPats
    PInfix lhs name rhs -> Set.insert <$> resolvedTermKey name <*> (Set.union <$> freeVarsPattern lhs <*> freeVarsPattern rhs)
    PRecord _ fields _ -> Set.unions <$> mapM (freeVarsPattern . recordFieldValue) fields
    PView viewExpr inner -> Set.union <$> freeVarsExpr viewExpr <*> freeVarsPattern inner
    _ -> pure Set.empty

freeVarsRhs :: Rhs Expr -> TcM (Set.Set TcTermKey)
freeVarsRhs rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls -> do
      exprVars <- freeVarsExpr expr
      declVars <- maybe (pure Set.empty) freeVarsDecls maybeDecls
      pure (exprVars <> declVars)
    GuardedRhss _ alternatives maybeDecls -> do
      altVars <- Set.unions <$> mapM freeVarsGuardedRhs alternatives
      declVars <- maybe (pure Set.empty) freeVarsDecls maybeDecls
      pure (altVars <> declVars)

freeVarsGuardedRhs :: GuardedRhs Expr -> TcM (Set.Set TcTermKey)
freeVarsGuardedRhs alternative =
  freeVarsGuardQualifiers (guardedRhsGuards alternative) (freeVarsExpr (guardedRhsBody alternative))

-- | The free variables of guard qualifiers and of the body they scope over.
-- A pattern guard or a let guard binds names for the later qualifiers.
freeVarsGuardQualifiers :: [GuardQualifier] -> TcM (Set.Set TcTermKey) -> TcM (Set.Set TcTermKey)
freeVarsGuardQualifiers qualifiers bodyVars =
  case qualifiers of
    [] -> bodyVars
    GuardAnn _ inner : rest -> freeVarsGuardQualifiers (inner : rest) bodyVars
    GuardExpr condition : rest ->
      Set.union <$> freeVarsExpr condition <*> freeVarsGuardQualifiers rest bodyVars
    GuardPat pat scrutinee : rest -> do
      scrutVars <- freeVarsExpr scrutinee
      patVars <- freeVarsPattern pat
      binders <- patternBinderKeys pat
      restVars <- freeVarsGuardQualifiers rest bodyVars
      pure (scrutVars <> patVars <> Set.difference restVars binders)
    GuardLet decls : rest -> do
      declVars <- freeVarsDecls decls
      localBinders <- declBinderKeys decls
      restVars <- freeVarsGuardQualifiers rest bodyVars
      pure (Set.difference (declVars <> restVars) localBinders)

freeVarsExpr :: Expr -> TcM (Set.Set TcTermKey)
freeVarsExpr expr =
  case expr of
    EVar name -> Set.singleton <$> resolvedTermKey name
    EAnn ann inner -> do
      innerVars <- freeVarsExpr inner
      insertSyntaxTermKey ann innerVars
    EIf a b c -> Set.unions <$> mapM freeVarsExpr [a, b, c]
    ELambdaPats pats body -> do
      bodyVars <- freeVarsExpr body
      patVars <- Set.unions <$> mapM freeVarsPattern pats
      binders <- Set.unions <$> mapM patternBinderKeys pats
      pure (Set.difference (bodyVars <> patVars) binders)
    EInfix lhs op rhs -> do
      lhsVars <- freeVarsExpr lhs
      rhsVars <- freeVarsExpr rhs
      opKey <- resolvedTermKey op
      pure (Set.insert opKey (lhsVars <> rhsVars))
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> do
      innerVars <- freeVarsExpr inner
      opKey <- resolvedTermKey op
      pure (Set.insert opKey innerVars)
    ESectionR op inner -> do
      innerVars <- freeVarsExpr inner
      opKey <- resolvedTermKey op
      pure (Set.insert opKey innerVars)
    ELetDecls decls body -> do
      declVars <- freeVarsDecls decls
      bodyVars <- freeVarsExpr body
      localBinders <- declBinderKeys decls
      pure (Set.difference (declVars <> bodyVars) localBinders)
    ECase scrut alts -> do
      scrutVars <- freeVarsExpr scrut
      altVars <- Set.unions <$> mapM freeVarsAlt alts
      pure (scrutVars <> altVars)
    ETypeSig inner _ -> freeVarsExpr inner
    EParen inner -> freeVarsExpr inner
    EPragma _ inner -> freeVarsExpr inner
    EList items -> Set.unions <$> mapM freeVarsExpr items
    EArithSeq arithSeq -> freeVarsArithSeq arithSeq
    ETuple _ items -> Set.unions <$> mapM (maybe (pure Set.empty) freeVarsExpr) items
    EApp f a -> do
      fVars <- freeVarsExpr f
      aVars <- freeVarsExpr a
      pure (fVars <> aVars)
    EDo stmts _ -> freeVarsDoStmts stmts
    _ -> pure Set.empty

-- | Add the syntax term that a resolver annotation names, if it names one.
--
-- RebindableSyntax can bind a syntax term such as @>>@ or @negate@ in the
-- same binding group, so the term is a dependency of the binding.
insertSyntaxTermKey :: Annotation -> Set.Set TcTermKey -> TcM (Set.Set TcTermKey)
insertSyntaxTermKey ann vars =
  case fromAnnotation ann :: Maybe ResolutionAnnotation of
    Just resolution
      | resolutionNamespace resolution == ResolutionNamespaceTerm,
        IdentifierNamed methodName <- resolutionIdentifier resolution -> do
          methodKey <- resolvedTargetTermKey methodName (resolutionTarget resolution)
          pure (Set.insert methodKey vars)
    _ -> pure vars

freeVarsDoStmts :: [DoStmt Expr] -> TcM (Set.Set TcTermKey)
freeVarsDoStmts stmts =
  case stmts of
    [] -> pure Set.empty
    stmt : rest ->
      case stmt of
        DoAnn ann inner -> do
          vars <- freeVarsDoStmts (inner : rest)
          insertSyntaxTermKey ann vars
        DoExpr body -> Set.union <$> freeVarsExpr body <*> freeVarsDoStmts rest
        DoBind pat body -> do
          bodyVars <- freeVarsExpr body
          patVars <- freeVarsPattern pat
          restVars <- freeVarsDoStmts rest
          binders <- patternBinderKeys pat
          pure (bodyVars <> patVars <> Set.difference restVars binders)
        DoLetDecls decls -> do
          declVars <- freeVarsDecls decls
          restVars <- freeVarsDoStmts rest
          binders <- declBinderKeys decls
          pure (Set.difference (declVars <> restVars) binders)
        DoRecStmt inner -> Set.union <$> freeVarsDoStmts inner <*> freeVarsDoStmts rest

freeVarsArithSeq :: ArithSeq -> TcM (Set.Set TcTermKey)
freeVarsArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> do
      innerVars <- freeVarsArithSeq inner
      insertSyntaxTermKey ann innerVars
    ArithSeqFrom from -> freeVarsExpr from
    ArithSeqFromThen from thenExpr -> Set.union <$> freeVarsExpr from <*> freeVarsExpr thenExpr
    ArithSeqFromTo from to -> Set.union <$> freeVarsExpr from <*> freeVarsExpr to
    ArithSeqFromThenTo from thenExpr to -> Set.unions <$> mapM freeVarsExpr [from, thenExpr, to]

freeVarsAlt :: CaseAlt Expr -> TcM (Set.Set TcTermKey)
freeVarsAlt (CaseAlt _ pat rhs) = do
  vars <- freeVarsRhs rhs
  patVars <- freeVarsPattern pat
  binders <- patternBinderKeys pat
  pure (Set.difference (vars <> patVars) binders)

declBinderKeys :: [Decl] -> TcM (Set.Set TcTermKey)
declBinderKeys decls =
  Set.unions <$> mapM declBinderKeySet decls

declBinderKeySet :: Decl -> TcM (Set.Set TcTermKey)
declBinderKeySet decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name _) -> Set.singleton <$> resolvedUnqualifiedTermKey name
    DeclValue (PatternBind _ pat _) -> patternBinderKeys pat
    _ -> pure Set.empty

patternBinderKeys :: Pattern -> TcM (Set.Set TcTermKey)
patternBinderKeys pat = Set.fromList <$> mapM resolvedUnqualifiedTermKey (patternBinderNames pat)

patternBinderKeyList :: Pattern -> TcM [TcTermKey]
patternBinderKeyList = mapM resolvedLocalTermKey . patternBinderNames

hasPartialTypeSig :: Decl -> Bool
hasPartialTypeSig decl =
  case peelDeclAnn decl of
    DeclTypeSig _ ty -> hasWildcardType ty
    _ -> False

hasWildcardType :: Type -> Bool
hasWildcardType ty =
  case ty of
    TWildcard -> True
    TApp f a -> hasWildcardType f || hasWildcardType a
    TFun _ a b -> hasWildcardType a || hasWildcardType b
    TParen inner -> hasWildcardType inner
    TAnn _ inner -> hasWildcardType inner
    TContext preds inner -> any hasWildcardType preds || hasWildcardType inner
    TForall _ inner -> hasWildcardType inner
    TTuple _ _ args -> any hasWildcardType args
    TList _ args -> any hasWildcardType args
    _ -> False
