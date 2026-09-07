-- | Make System FC local names easier to read.
module Aihc.Fc.Tidy
  ( tidyProgram,
    tidyProgramWithTidiedImports,
    tidyTypeEnv,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv (..))
import Aihc.Tc.Types (Unique (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data TidyEnv = TidyEnv
  { tidyNames :: Map Name Name,
    tidyUsed :: Map Text (Set Int)
  }

emptyTidyEnv :: TidyEnv
emptyTidyEnv = TidyEnv Map.empty Map.empty

-- | Give each local name the lowest number that its lexical scope permits.
tidyProgram :: Program -> Program
tidyProgram program =
  program
    { programImports = tidyImports (programImports program),
      programDecls = map tidyDecl (programDecls program)
    }

tidyProgramWithTidiedImports :: Program -> Program
tidyProgramWithTidiedImports program =
  program {programDecls = map tidyDecl (programDecls program)}

tidyTypeEnv :: TypeEnv -> TypeEnv
tidyTypeEnv env =
  env
    { teHeaders = Map.map (tidyType emptyTidyEnv) (teHeaders env),
      teSynonyms = Map.map (tidyType emptyTidyEnv) (teSynonyms env),
      teAxioms = Map.map tidyAxiomDecl (teAxioms env),
      teBinders = Map.map (tidyType emptyTidyEnv) (teBinders env)
    }

tidyImports :: Imports -> Imports
tidyImports imports =
  imports
    { importHeaders = Map.map (tidyType emptyTidyEnv) (importHeaders imports),
      importSynonyms = Map.map (tidyType emptyTidyEnv) (importSynonyms imports),
      importAxioms = Map.map tidyAxiomDecl (importAxioms imports),
      importBinders = Map.map (tidyType emptyTidyEnv) (importBinders imports)
    }

tidyDecl :: Decl -> Decl
tidyDecl decl =
  case decl of
    DeclType declaration ->
      let (binders, env) = tidyBinders emptyTidyEnv (typeBinders declaration)
       in DeclType
            declaration
              { typeBinders = binders,
                typeResult = tidyType env (typeResult declaration),
                typeCons = map tidyConDecl (typeCons declaration)
              }
    DeclSynonym declaration ->
      let (binders, env) = tidyBinders emptyTidyEnv (synBinders declaration)
       in DeclSynonym
            declaration
              { synBinders = binders,
                synResult = tidyType env (synResult declaration),
                synBody = tidyType env (synBody declaration)
              }
    DeclAxiom declaration -> DeclAxiom (tidyAxiomDecl declaration)
    DeclVal declaration ->
      DeclVal
        declaration
          { valType = tidyType emptyTidyEnv (valType declaration),
            valBody = tidyExpr emptyTidyEnv (valBody declaration)
          }

tidyConDecl :: ConDecl -> ConDecl
tidyConDecl declaration =
  declaration {conType = tidyType emptyTidyEnv (conType declaration)}

tidyAxiomDecl :: AxiomDecl -> AxiomDecl
tidyAxiomDecl declaration =
  let (binders, env) = tidyBinders emptyTidyEnv (axiomBinders declaration)
   in declaration
        { axiomBinders = binders,
          axiomLeft = tidyType env (axiomLeft declaration),
          axiomRight = tidyType env (axiomRight declaration)
        }

tidyType :: TidyEnv -> Type -> Type
tidyType env ty =
  case ty of
    TyVar name -> TyVar (tidyUse env name)
    TyCon name -> TyCon (tidyUse env name)
    TyApp function argument -> TyApp (tidyType env function) (tidyType env argument)
    TyFun r1 r2 argument result ->
      TyFun (tidyType env r1) (tidyType env r2) (tidyType env argument) (tidyType env result)
    TyForAll binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in TyForAll binder' (tidyType bodyEnv body)
    TyEq left right -> TyEq (tidyType env left) (tidyType env right)

tidyExpr :: TidyEnv -> Expr -> Expr
tidyExpr env expr =
  case expr of
    ExVar name -> ExVar (tidyUse env name)
    ExLit literal -> ExLit (tidyLiteral env literal)
    ExApp function argument -> ExApp (tidyExpr env function) (tidyExpr env argument)
    ExTyApp function argument -> ExTyApp (tidyExpr env function) (tidyType env argument)
    ExForeignCall call types arguments ->
      ExForeignCall
        -- The foreign type is closed, but its binders take names that no
        -- enclosing binder has, so that no binder of the declaration repeats.
        call {foreignCallType = tidyType env (foreignCallType call)}
        (map (tidyType env) types)
        (map (tidyExpr env) arguments)
    ExLam binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in ExLam binder' (tidyExpr bodyEnv body)
    ExTyLam binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in ExTyLam binder' (tidyExpr bodyEnv body)
    ExLet bind body ->
      let (binder', bodyEnv) = tidyBinder env (bindBinder bind)
          bind' = Bind binder' (tidyExpr env (bindRhs bind))
       in ExLet bind' (tidyExpr bodyEnv body)
    ExRec binds body ->
      let (binders, bodyEnv) = tidyBinders env (map bindBinder binds)
          binds' = zipWith (tidyRecBind bodyEnv) binders binds
       in ExRec binds' (tidyExpr bodyEnv body)
    ExCase scrutinee binder resultType alternatives ->
      let (binder', caseEnv) = tidyBinder env binder
       in ExCase
            (tidyExpr env scrutinee)
            binder'
            (tidyType env resultType)
            (map (tidyAlt caseEnv) alternatives)
    ExCoercion proof -> ExCoercion (tidyCoercion env proof)
    ExCast body coercion -> ExCast (tidyExpr env body) (tidyCoercion env coercion)

tidyRecBind :: TidyEnv -> Binder -> Bind -> Bind
tidyRecBind env binder bind =
  Bind binder (tidyExpr env (bindRhs bind))

tidyAlt :: TidyEnv -> Alt -> Alt
tidyAlt env alternative =
  let (typeBinders, typeEnv) = tidyBinders env (altTypeBinders alternative)
      (binders, rhsEnv) = tidyBinders typeEnv (altBinders alternative)
   in alternative
        { altCon = tidyAltCon env (altCon alternative),
          altTypeBinders = typeBinders,
          altBinders = binders,
          altRhs = tidyExpr rhsEnv (altRhs alternative)
        }

tidyAltCon :: TidyEnv -> AltCon -> AltCon
tidyAltCon env alternative =
  case alternative of
    AltData name -> AltData (tidyUse env name)
    AltLit literal -> AltLit (tidyLiteral env literal)
    AltDefault -> AltDefault

tidyLiteral :: TidyEnv -> Literal -> Literal
tidyLiteral env literal =
  case literal of
    LitInt representation value -> LitInt (tidyType env representation) value
    LitChar representation value -> LitChar (tidyType env representation) value
    LitAddr representation value -> LitAddr (tidyType env representation) value

tidyCoercion :: TidyEnv -> Coercion -> Coercion
tidyCoercion env coercion =
  case coercion of
    CoVar name -> CoVar (tidyUse env name)
    CoRefl ty -> CoRefl (tidyType env ty)
    CoSym inner -> CoSym (tidyCoercion env inner)
    CoTrans left right -> CoTrans (tidyCoercion env left) (tidyCoercion env right)
    CoApp function argument -> CoApp (tidyCoercion env function) (tidyCoercion env argument)
    CoNth index proof -> CoNth index (tidyCoercion env proof)
    CoFun domain range -> CoFun (tidyCoercion env domain) (tidyCoercion env range)
    CoTyConApp name arguments ->
      CoTyConApp (tidyUse env name) (map (tidyCoercion env) arguments)
    CoAxiom name arguments ->
      CoAxiom (tidyUse env name) (map (tidyType env) arguments)

tidyBinders :: TidyEnv -> [Binder] -> ([Binder], TidyEnv)
tidyBinders env binders =
  case binders of
    [] -> ([], env)
    binder : rest ->
      let (binder', nextEnv) = tidyBinder env binder
          (rest', finalEnv) = tidyBinders nextEnv rest
       in (binder' : rest', finalEnv)

tidyBinder :: TidyEnv -> Binder -> (Binder, TidyEnv)
tidyBinder env binder =
  let oldName = binderName binder
      newName = tidyBinderName env oldName
      binder' = Binder newName (tidyType env (binderType binder))
   in (binder', bindName env oldName newName)

tidyBinderName :: TidyEnv -> Name -> Name
tidyBinderName env name =
  case nameOrigin name of
    OriginLocal {} -> name {nameOrigin = OriginLocal (Unique (lowestUnused used))}
      where
        used = Map.findWithDefault Set.empty (nameText name) (tidyUsed env)
    OriginTop {} -> name

bindName :: TidyEnv -> Name -> Name -> TidyEnv
bindName env oldName newName =
  case nameOrigin newName of
    OriginLocal (Unique unique) ->
      env
        { tidyNames = Map.insert oldName newName (tidyNames env),
          tidyUsed = Map.insertWith Set.union (nameText newName) (Set.singleton unique) (tidyUsed env)
        }
    OriginTop {} -> env

tidyUse :: TidyEnv -> Name -> Name
tidyUse env name =
  case nameOrigin name of
    OriginLocal {} -> Map.findWithDefault name name (tidyNames env)
    OriginTop {} -> name

lowestUnused :: Set Int -> Int
lowestUnused used = go 0
  where
    go candidate
      | candidate `Set.member` used = go (candidate + 1)
      | otherwise = candidate
