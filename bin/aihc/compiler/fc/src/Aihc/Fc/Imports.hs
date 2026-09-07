-- | Select and check the facts that a System FC program imports.
module Aihc.Fc.Imports
  ( emptyImports,
    importsForProgramLookup,
    unusedImports,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf
  ( TypeEnv (..),
    emptyTypeEnv,
    typeEnvFromProgram,
    typeHead,
    unionTypeEnv,
  )
import Aihc.Resolve (PackageId)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type References = Set Name

emptyImports :: Imports
emptyImports = Imports Map.empty Map.empty Map.empty Map.empty

-- | Convert each used name when the import closure first needs it.
importsForProgramLookup :: PackageId -> (Name -> Either String (Maybe TypeEnv)) -> Program -> Either String Imports
importsForProgramLookup primPackage lookupFact program = do
  let localProgram = program {programImports = emptyImports}
      localNames = namesInTypeEnv (typeEnvFromProgram primPackage localProgram)
      roots =
        ( foldMap declReferences (programDecls program)
            <> referencesFromImports (programImports program)
        )
          `Set.difference` localNames
  (available, selectedNames) <- closeLookup lookupFact localNames primPackage roots
  let selectedImports = importsForNames available selectedNames
  pure (mergeImports selectedImports (programImports program))

closeLookup ::
  (Name -> Either String (Maybe TypeEnv)) ->
  Set Name ->
  PackageId ->
  Set Name ->
  Either String (TypeEnv, Set Name)
closeLookup lookupFact localNames primPackage = go Set.empty Set.empty (emptyTypeEnv primPackage)
  where
    go visited selected env pending =
      case Set.minView pending of
        Nothing -> Right (env, selected)
        Just (name, rest)
          | Set.member name visited -> go visited selected env rest
          | Set.member name localNames -> go (Set.insert name visited) selected env rest
          | nameInTypeEnv env name ->
              go
                (Set.insert name visited)
                (Set.insert name selected)
                env
                (rest <> (referencesForName env name `Set.difference` localNames `Set.difference` visited))
          | otherwise ->
              case lookupFact name of
                Left message -> Left message
                Right Nothing -> go (Set.insert name visited) selected env rest
                Right (Just fragment) ->
                  let env' = unionTypeEnv env fragment
                      newNames = referencesForName env' name `Set.difference` localNames `Set.difference` visited
                   in go (Set.insert name visited) (Set.insert name selected) env' (rest <> newNames)

nameInTypeEnv :: TypeEnv -> Name -> Bool
nameInTypeEnv env name =
  Map.member name (teHeaders env)
    || Map.member name (teSynonyms env)
    || Map.member name (teAxioms env)
    || Map.member name (teBinders env)

referencesForName :: TypeEnv -> Name -> References
referencesForName available name =
  foldMap typeReferences (Map.lookup name (teHeaders available))
    <> foldMap typeReferences (Map.lookup name (teSynonyms available))
    <> foldMap axiomReferences (Map.lookup name (teAxioms available))
    <> foldMap typeReferences (Map.lookup name (teBinders available))
    <> familyEquationNames available name

-- | The equations of a type family. A use of the family imports them, so
-- the applications of the family reduce.
familyEquationNames :: TypeEnv -> Name -> References
familyEquationNames available family =
  Set.fromList
    [ axiomName axiom
    | axiom <- Map.elems (teAxioms available),
      axiomRole axiom == Nominal,
      typeHead (axiomLeft axiom) == Just family
    ]

importsForNames :: TypeEnv -> Set Name -> Imports
importsForNames available names =
  Imports
    { importHeaders = Map.restrictKeys (teHeaders available) names,
      importSynonyms = Map.restrictKeys (teSynonyms available) names,
      importAxioms = Map.restrictKeys (teAxioms available) names,
      importBinders = Map.restrictKeys (teBinders available) names
    }

mergeImports :: Imports -> Imports -> Imports
mergeImports preferred fallback =
  Imports
    { importHeaders = Map.union (importHeaders preferred) (importHeaders fallback),
      importSynonyms = Map.union (importSynonyms preferred) (importSynonyms fallback),
      importAxioms = Map.union (importAxioms preferred) (importAxioms fallback),
      importBinders = Map.union (importBinders preferred) (importBinders fallback)
    }

-- | Return one entry for each import declaration that has no use.
unusedImports :: Program -> [Name]
unusedImports program =
  filter (`Set.notMember` usedNames) importNames
  where
    imports = programImports program
    directReferences = foldMap declReferences (programDecls program)
    importReferences = referencesFromImports imports
    referencedNames = directReferences <> importReferences
    usedNames = referencedNames <> familyEquations
    -- An equation of a referenced family is in use through the family.
    familyEquations =
      Set.fromList
        [ name
        | (name, axiom) <- Map.toList (importAxioms imports),
          axiomRole axiom == Nominal,
          Just family <- [typeHead (axiomLeft axiom)],
          Set.member family referencedNames
        ]
    importNames =
      Map.keys (importHeaders imports)
        <> Map.keys (importSynonyms imports)
        <> Map.keys (importAxioms imports)
        <> Map.keys (importBinders imports)

namesInTypeEnv :: TypeEnv -> Set Name
namesInTypeEnv env =
  Map.keysSet (teHeaders env)
    <> Map.keysSet (teSynonyms env)
    <> Map.keysSet (teAxioms env)
    <> Map.keysSet (teBinders env)

referencesFromImports :: Imports -> References
referencesFromImports imports =
  foldMap (uncurry entryTypeReferences) (Map.toList (importHeaders imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importSynonyms imports))
    <> foldMap (uncurry entryAxiomReferences) (Map.toList (importAxioms imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importBinders imports))

entryTypeReferences :: Name -> Type -> References
entryTypeReferences name = Set.delete name . typeReferences

entryAxiomReferences :: Name -> AxiomDecl -> References
entryAxiomReferences name = Set.delete name . axiomReferences

declReferences :: Decl -> References
declReferences decl =
  case decl of
    DeclType declaration ->
      foldMap binderReferences (typeBinders declaration)
        <> typeReferences (typeResult declaration)
        <> foldMap (typeReferences . conType) (typeCons declaration)
    DeclSynonym declaration ->
      foldMap binderReferences (synBinders declaration)
        <> typeReferences (synResult declaration)
        <> typeReferences (synBody declaration)
    DeclAxiom declaration -> axiomReferences declaration
    DeclVal declaration -> typeReferences (valType declaration) <> exprReferences (valBody declaration)

foreignImportDependencyReferences :: ForeignImportDependency -> References
foreignImportDependencyReferences dependency =
  case dependency of
    ForeignAxiom name -> nameReference name
    ForeignConstructor name -> nameReference name

axiomReferences :: AxiomDecl -> References
axiomReferences declaration =
  foldMap binderReferences (axiomBinders declaration)
    <> typeReferences (axiomLeft declaration)
    <> typeReferences (axiomRight declaration)

binderReferences :: Binder -> References
binderReferences = typeReferences . binderType

typeReferences :: Type -> References
typeReferences ty =
  case ty of
    TyVar name -> nameReference name
    TyCon name -> nameReference name
    TyApp function argument -> typeReferences function <> typeReferences argument
    TyFun r1 r2 argument result -> foldMap typeReferences [r1, r2, argument, result]
    TyForAll binder body -> binderReferences binder <> typeReferences body
    TyEq left right -> typeReferences left <> typeReferences right

exprReferences :: Expr -> References
exprReferences expr =
  case expr of
    ExVar name -> nameReference name
    ExLit literal -> literalReferences literal
    ExApp function argument -> exprReferences function <> exprReferences argument
    ExTyApp function argument -> exprReferences function <> typeReferences argument
    ExLam binder body -> binderReferences binder <> exprReferences body
    ExTyLam binder body -> binderReferences binder <> exprReferences body
    ExLet binding body -> bindReferences binding <> exprReferences body
    ExRec bindings body -> foldMap bindReferences bindings <> exprReferences body
    ExCase scrutinee binder result alts ->
      exprReferences scrutinee
        <> binderReferences binder
        <> typeReferences result
        <> foldMap altReferences alts
    ExCoercion proof -> coercionReferences proof
    ExCast body coercion -> exprReferences body <> coercionReferences coercion
    ExForeignCall call types arguments ->
      foreignCallReferences call
        <> foldMap typeReferences types
        <> foldMap exprReferences arguments

-- | A foreign call carries its own type, so the imported name of the foreign
-- import is not a reference. Its type and its dependencies are.
foreignCallReferences :: ForeignCall -> References
foreignCallReferences call =
  typeReferences (foreignCallType call)
    <> foldMap foreignImportDependencyReferences (foreignCallDependencies call)

bindReferences :: Bind -> References
bindReferences binding = binderReferences (bindBinder binding) <> exprReferences (bindRhs binding)

altReferences :: Alt -> References
altReferences alternative =
  altConReferences (altCon alternative)
    <> foldMap binderReferences (altTypeBinders alternative)
    <> foldMap binderReferences (altBinders alternative)
    <> exprReferences (altRhs alternative)

altConReferences :: AltCon -> References
altConReferences altCon =
  case altCon of
    AltData name -> nameReference name
    AltLit literal -> literalReferences literal
    AltDefault -> mempty

literalReferences :: Literal -> References
literalReferences literal =
  case literal of
    LitInt representation _ -> typeReferences representation
    LitChar representation _ -> typeReferences representation
    LitAddr representation _ -> typeReferences representation

coercionReferences :: Coercion -> References
coercionReferences coercion =
  case coercion of
    CoVar name -> nameReference name
    CoRefl ty -> typeReferences ty
    CoSym inner -> coercionReferences inner
    CoTrans left right -> coercionReferences left <> coercionReferences right
    CoTyConApp name arguments -> nameReference name <> foldMap coercionReferences arguments
    CoAxiom name arguments -> nameReference name <> foldMap typeReferences arguments

nameReference :: Name -> References
nameReference = Set.singleton
