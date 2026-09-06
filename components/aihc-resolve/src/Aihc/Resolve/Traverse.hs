-- | A hand-written walk over the annotations of the parser syntax tree.
--
-- The resolver and the type checker read and rewrite the annotations of a
-- whole module several times. A generic "Data.Data" walk does this work, but it visits
-- every field of every node and does a runtime type test at each one. This
-- module walks the same tree with one case for each constructor. The walk
-- visits the annotations in source order: the fields of a constructor from
-- left to right, and the elements of a list from first to last.
--
-- The pattern matches are exhaustive without a wildcard. A new syntax
-- constructor therefore fails the build until its case is added.
module Aihc.Resolve.Traverse
  ( HasAnnotations (..),
    annotationList,
  )
where

import Aihc.Parser.Syntax
import Data.Functor.Const (Const (..))

-- | Syntax that can hold annotations.
class HasAnnotations a where
  -- | Apply an effect to every annotation, in source order.
  traverseAnnotations :: (Applicative f) => (Annotation -> f Annotation) -> a -> f a

-- | Every annotation of a piece of syntax, in source order.
annotationList :: (HasAnnotations a) => a -> [Annotation]
annotationList = getConst . traverseAnnotations (\ann -> Const [ann])

instance HasAnnotations Annotation where
  traverseAnnotations f = f

instance (HasAnnotations a) => HasAnnotations [a] where
  traverseAnnotations = traverse . traverseAnnotations

instance (HasAnnotations a) => HasAnnotations (Maybe a) where
  traverseAnnotations = traverse . traverseAnnotations

instance (HasAnnotations a, HasAnnotations b) => HasAnnotations (Either a b) where
  traverseAnnotations f = either (fmap Left . traverseAnnotations f) (fmap Right . traverseAnnotations f)

instance (HasAnnotations a, HasAnnotations b) => HasAnnotations (a, b) where
  traverseAnnotations f (left, right) = (,) <$> traverseAnnotations f left <*> traverseAnnotations f right

-- Names

instance HasAnnotations Name where
  traverseAnnotations f name = (\anns -> name {nameAnns = anns}) <$> traverseAnnotations f (nameAnns name)

instance HasAnnotations UnqualifiedName where
  traverseAnnotations f name = (\anns -> name {unqualifiedNameAnns = anns}) <$> traverseAnnotations f (unqualifiedNameAnns name)

-- Modules, exports, and imports

instance HasAnnotations Module where
  traverseAnnotations f (Module anns modHead pragmas imports decls) =
    Module
      <$> traverseAnnotations f anns
      <*> traverseAnnotations f modHead
      <*> pure pragmas
      <*> traverseAnnotations f imports
      <*> traverseAnnotations f decls

instance HasAnnotations ModuleHead where
  traverseAnnotations f (ModuleHead anns name warning exports) =
    ModuleHead <$> traverseAnnotations f anns <*> pure name <*> pure warning <*> traverseAnnotations f exports

instance HasAnnotations IEBundledMember where
  traverseAnnotations f (IEBundledMember namespace name) =
    IEBundledMember namespace <$> traverseAnnotations f name

instance HasAnnotations ExportSpec where
  traverseAnnotations f spec =
    case spec of
      ExportModule pragma name -> pure (ExportModule pragma name)
      ExportVar pragma namespace name -> ExportVar pragma namespace <$> traverseAnnotations f name
      ExportAbs pragma namespace name -> ExportAbs pragma namespace <$> traverseAnnotations f name
      ExportAll pragma namespace name -> ExportAll pragma namespace <$> traverseAnnotations f name
      ExportWith pragma namespace name members ->
        ExportWith pragma namespace <$> traverseAnnotations f name <*> traverseAnnotations f members
      ExportWithAll pragma namespace name position members ->
        ExportWithAll pragma namespace <$> traverseAnnotations f name <*> pure position <*> traverseAnnotations f members
      ExportAnn ann inner -> ExportAnn <$> f ann <*> traverseAnnotations f inner

instance HasAnnotations ImportDecl where
  traverseAnnotations f decl =
    (\anns spec -> decl {importDeclAnns = anns, importDeclSpec = spec})
      <$> traverseAnnotations f (importDeclAnns decl)
      <*> traverseAnnotations f (importDeclSpec decl)

instance HasAnnotations ImportSpec where
  traverseAnnotations f (ImportSpec anns hiding items) =
    ImportSpec <$> traverseAnnotations f anns <*> pure hiding <*> traverseAnnotations f items

instance HasAnnotations ImportItem where
  traverseAnnotations f item =
    case item of
      ImportItemVar namespace name -> ImportItemVar namespace <$> traverseAnnotations f name
      ImportItemAbs namespace name -> ImportItemAbs namespace <$> traverseAnnotations f name
      ImportItemAll namespace name -> ImportItemAll namespace <$> traverseAnnotations f name
      ImportItemWith namespace name members ->
        ImportItemWith namespace <$> traverseAnnotations f name <*> traverseAnnotations f members
      ImportItemAllWith namespace name position members ->
        ImportItemAllWith namespace <$> traverseAnnotations f name <*> pure position <*> traverseAnnotations f members
      ImportAnn ann inner -> ImportAnn <$> f ann <*> traverseAnnotations f inner

-- Declarations

instance HasAnnotations Decl where
  traverseAnnotations f decl =
    case decl of
      DeclAnn ann inner -> DeclAnn <$> f ann <*> traverseAnnotations f inner
      DeclValue value -> DeclValue <$> traverseAnnotations f value
      DeclImplicitParam name expr decls ->
        DeclImplicitParam name <$> traverseAnnotations f expr <*> traverseAnnotations f decls
      DeclTypeSig names ty -> DeclTypeSig <$> traverseAnnotations f names <*> traverseAnnotations f ty
      DeclPatSyn patSyn -> DeclPatSyn <$> traverseAnnotations f patSyn
      DeclPatSynSig names ty -> DeclPatSynSig <$> traverseAnnotations f names <*> traverseAnnotations f ty
      DeclStandaloneKindSig name ty -> DeclStandaloneKindSig <$> traverseAnnotations f name <*> traverseAnnotations f ty
      DeclFixity assoc namespace precedence operators ->
        DeclFixity assoc namespace precedence <$> traverseAnnotations f operators
      DeclRoleAnnotation roles -> DeclRoleAnnotation <$> traverseAnnotations f roles
      DeclTypeSyn synonym -> DeclTypeSyn <$> traverseAnnotations f synonym
      DeclTypeData dataDecl -> DeclTypeData <$> traverseAnnotations f dataDecl
      DeclData dataDecl -> DeclData <$> traverseAnnotations f dataDecl
      DeclNewtype newtypeDecl -> DeclNewtype <$> traverseAnnotations f newtypeDecl
      DeclClass classDecl -> DeclClass <$> traverseAnnotations f classDecl
      DeclInstance instanceDecl -> DeclInstance <$> traverseAnnotations f instanceDecl
      DeclStandaloneDeriving derivingDecl -> DeclStandaloneDeriving <$> traverseAnnotations f derivingDecl
      DeclDefault types -> DeclDefault <$> traverseAnnotations f types
      DeclSplice expr -> DeclSplice <$> traverseAnnotations f expr
      DeclForeign foreignDecl -> DeclForeign <$> traverseAnnotations f foreignDecl
      DeclTypeFamilyDecl familyDecl -> DeclTypeFamilyDecl <$> traverseAnnotations f familyDecl
      DeclDataFamilyDecl familyDecl -> DeclDataFamilyDecl <$> traverseAnnotations f familyDecl
      DeclTypeFamilyInst familyInst -> DeclTypeFamilyInst <$> traverseAnnotations f familyInst
      DeclDataFamilyInst familyInst -> DeclDataFamilyInst <$> traverseAnnotations f familyInst
      DeclPragma pragma -> pure (DeclPragma pragma)

instance HasAnnotations ValueDecl where
  traverseAnnotations f value =
    case value of
      FunctionBind name matches -> FunctionBind <$> traverseAnnotations f name <*> traverseAnnotations f matches
      PatternBind multiplicity pat rhs ->
        PatternBind <$> traverseAnnotations f multiplicity <*> traverseAnnotations f pat <*> traverseAnnotations f rhs

instance HasAnnotations MultiplicityTag where
  traverseAnnotations f tag =
    case tag of
      NoMultiplicityTag -> pure NoMultiplicityTag
      LinearMultiplicityTag -> pure LinearMultiplicityTag
      ExplicitMultiplicityTag ty -> ExplicitMultiplicityTag <$> traverseAnnotations f ty

instance HasAnnotations Match where
  traverseAnnotations f (Match anns headForm pats rhs) =
    Match <$> traverseAnnotations f anns <*> pure headForm <*> traverseAnnotations f pats <*> traverseAnnotations f rhs

instance HasAnnotations PatSynDecl where
  traverseAnnotations f (PatSynDecl name args pat direction) =
    PatSynDecl <$> traverseAnnotations f name <*> pure args <*> traverseAnnotations f pat <*> traverseAnnotations f direction

instance HasAnnotations PatSynDir where
  traverseAnnotations f direction =
    case direction of
      PatSynUnidirectional -> pure PatSynUnidirectional
      PatSynBidirectional -> pure PatSynBidirectional
      PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> traverseAnnotations f matches

instance (HasAnnotations body) => HasAnnotations (Rhs body) where
  traverseAnnotations f rhs =
    case rhs of
      UnguardedRhs anns body decls ->
        UnguardedRhs <$> traverseAnnotations f anns <*> traverseAnnotations f body <*> traverseAnnotations f decls
      GuardedRhss anns guarded decls ->
        GuardedRhss <$> traverseAnnotations f anns <*> traverseAnnotations f guarded <*> traverseAnnotations f decls

instance (HasAnnotations body) => HasAnnotations (GuardedRhs body) where
  traverseAnnotations f (GuardedRhs anns guards body) =
    GuardedRhs <$> traverseAnnotations f anns <*> traverseAnnotations f guards <*> traverseAnnotations f body

instance HasAnnotations GuardQualifier where
  traverseAnnotations f qualifier =
    case qualifier of
      GuardAnn ann inner -> GuardAnn <$> f ann <*> traverseAnnotations f inner
      GuardExpr expr -> GuardExpr <$> traverseAnnotations f expr
      GuardPat pat expr -> GuardPat <$> traverseAnnotations f pat <*> traverseAnnotations f expr
      GuardLet decls -> GuardLet <$> traverseAnnotations f decls

instance HasAnnotations Literal where
  traverseAnnotations f literal =
    case literal of
      LitAnn ann inner -> LitAnn <$> f ann <*> traverseAnnotations f inner
      LitInt {} -> pure literal
      LitFloat {} -> pure literal
      LitChar {} -> pure literal
      LitCharHash {} -> pure literal
      LitString {} -> pure literal
      LitStringHash {} -> pure literal

-- Patterns

instance (HasAnnotations a) => HasAnnotations (RecordField a) where
  traverseAnnotations f (RecordField name value pun) =
    RecordField <$> traverseAnnotations f name <*> traverseAnnotations f value <*> pure pun

instance HasAnnotations Pattern where
  traverseAnnotations f pat =
    case pat of
      PAnn ann inner -> PAnn <$> f ann <*> traverseAnnotations f inner
      PVar name -> PVar <$> traverseAnnotations f name
      PTypeBinder binder -> PTypeBinder <$> traverseAnnotations f binder
      PTypeSyntax form ty -> PTypeSyntax form <$> traverseAnnotations f ty
      PWildcard -> pure PWildcard
      PLit literal -> PLit <$> traverseAnnotations f literal
      PQuasiQuote quoter body -> pure (PQuasiQuote quoter body)
      PTuple flavor items -> PTuple flavor <$> traverseAnnotations f items
      PUnboxedSum position arity inner -> PUnboxedSum position arity <$> traverseAnnotations f inner
      PList items -> PList <$> traverseAnnotations f items
      PCon name types pats -> PCon <$> traverseAnnotations f name <*> traverseAnnotations f types <*> traverseAnnotations f pats
      PBuiltinCon builtin types pats -> PBuiltinCon builtin <$> traverseAnnotations f types <*> traverseAnnotations f pats
      PInfix lhs name rhs -> PInfix <$> traverseAnnotations f lhs <*> traverseAnnotations f name <*> traverseAnnotations f rhs
      PView expr inner -> PView <$> traverseAnnotations f expr <*> traverseAnnotations f inner
      PAs name inner -> PAs <$> traverseAnnotations f name <*> traverseAnnotations f inner
      PStrict inner -> PStrict <$> traverseAnnotations f inner
      PIrrefutable inner -> PIrrefutable <$> traverseAnnotations f inner
      PNegLit literal -> PNegLit <$> traverseAnnotations f literal
      PParen inner -> PParen <$> traverseAnnotations f inner
      PRecord name fields wildcard -> PRecord <$> traverseAnnotations f name <*> traverseAnnotations f fields <*> pure wildcard
      PTypeSig inner ty -> PTypeSig <$> traverseAnnotations f inner <*> traverseAnnotations f ty
      PSplice expr -> PSplice <$> traverseAnnotations f expr

-- Types

instance HasAnnotations ForallTelescope where
  traverseAnnotations f (ForallTelescope visibility binders) =
    ForallTelescope visibility <$> traverseAnnotations f binders

instance HasAnnotations ArrowKind where
  traverseAnnotations f arrow =
    case arrow of
      ArrowUnrestricted -> pure ArrowUnrestricted
      ArrowLinear -> pure ArrowLinear
      ArrowExplicit ty -> ArrowExplicit <$> traverseAnnotations f ty

instance HasAnnotations Type where
  traverseAnnotations f ty =
    case ty of
      TAnn ann inner -> TAnn <$> f ann <*> traverseAnnotations f inner
      TVar name -> TVar <$> traverseAnnotations f name
      TCon name promotion -> TCon <$> traverseAnnotations f name <*> pure promotion
      TBuiltinCon builtin promotion -> pure (TBuiltinCon builtin promotion)
      TImplicitParam name payload -> TImplicitParam name <$> traverseAnnotations f payload
      TTypeLit literal -> pure (TTypeLit literal)
      TStar text -> pure (TStar text)
      TQuasiQuote quoter body -> pure (TQuasiQuote quoter body)
      TForall telescope inner -> TForall <$> traverseAnnotations f telescope <*> traverseAnnotations f inner
      TApp function argument -> TApp <$> traverseAnnotations f function <*> traverseAnnotations f argument
      TTypeApp function argument -> TTypeApp <$> traverseAnnotations f function <*> traverseAnnotations f argument
      TInfix lhs name promotion rhs ->
        TInfix <$> traverseAnnotations f lhs <*> traverseAnnotations f name <*> pure promotion <*> traverseAnnotations f rhs
      TFun arrow argument result ->
        TFun <$> traverseAnnotations f arrow <*> traverseAnnotations f argument <*> traverseAnnotations f result
      TTuple flavor promotion items -> TTuple flavor promotion <$> traverseAnnotations f items
      TUnboxedSum items -> TUnboxedSum <$> traverseAnnotations f items
      TList promotion items -> TList promotion <$> traverseAnnotations f items
      TParen inner -> TParen <$> traverseAnnotations f inner
      TKindSig inner kind -> TKindSig <$> traverseAnnotations f inner <*> traverseAnnotations f kind
      TContext context inner -> TContext <$> traverseAnnotations f context <*> traverseAnnotations f inner
      TSplice expr -> TSplice <$> traverseAnnotations f expr
      TWildcard -> pure TWildcard

instance HasAnnotations TyVarBinder where
  traverseAnnotations f binder =
    (\anns kind -> binder {tyVarBinderAnns = anns, tyVarBinderKind = kind})
      <$> traverseAnnotations f (tyVarBinderAnns binder)
      <*> traverseAnnotations f (tyVarBinderKind binder)

instance (HasAnnotations name) => HasAnnotations (BinderHead name) where
  traverseAnnotations f binderHead =
    case binderHead of
      PrefixBinderHead name params -> PrefixBinderHead <$> traverseAnnotations f name <*> traverseAnnotations f params
      InfixBinderHead lhs name rhs params ->
        InfixBinderHead
          <$> traverseAnnotations f lhs
          <*> traverseAnnotations f name
          <*> traverseAnnotations f rhs
          <*> traverseAnnotations f params

-- Type declarations

instance HasAnnotations RoleAnnotation where
  traverseAnnotations f (RoleAnnotation name roles) =
    RoleAnnotation <$> traverseAnnotations f name <*> pure roles

instance HasAnnotations TypeSynDecl where
  traverseAnnotations f (TypeSynDecl synHead body) =
    TypeSynDecl <$> traverseAnnotations f synHead <*> traverseAnnotations f body

instance HasAnnotations TypeFamilyDecl where
  traverseAnnotations f (TypeFamilyDecl headForm explicitKeyword familyHead params resultSig equations) =
    TypeFamilyDecl headForm explicitKeyword
      <$> traverseAnnotations f familyHead
      <*> traverseAnnotations f params
      <*> traverseAnnotations f resultSig
      <*> traverseAnnotations f equations

instance HasAnnotations TypeFamilyResultSig where
  traverseAnnotations f resultSig =
    case resultSig of
      TypeFamilyKindSig kind -> TypeFamilyKindSig <$> traverseAnnotations f kind
      TypeFamilyTyVarSig binder -> TypeFamilyTyVarSig <$> traverseAnnotations f binder
      TypeFamilyInjectiveSig binder injectivity ->
        TypeFamilyInjectiveSig <$> traverseAnnotations f binder <*> traverseAnnotations f injectivity

instance HasAnnotations TypeFamilyInjectivity where
  traverseAnnotations f injectivity =
    (\anns -> injectivity {typeFamilyInjectivityAnns = anns})
      <$> traverseAnnotations f (typeFamilyInjectivityAnns injectivity)

instance HasAnnotations TypeFamilyEq where
  traverseAnnotations f (TypeFamilyEq anns binders headForm lhs rhs) =
    TypeFamilyEq
      <$> traverseAnnotations f anns
      <*> traverseAnnotations f binders
      <*> pure headForm
      <*> traverseAnnotations f lhs
      <*> traverseAnnotations f rhs

instance HasAnnotations DataFamilyDecl where
  traverseAnnotations f (DataFamilyDecl familyHead kind) =
    DataFamilyDecl <$> traverseAnnotations f familyHead <*> traverseAnnotations f kind

instance HasAnnotations TypeFamilyInst where
  traverseAnnotations f (TypeFamilyInst binders headForm lhs rhs) =
    TypeFamilyInst <$> traverseAnnotations f binders <*> pure headForm <*> traverseAnnotations f lhs <*> traverseAnnotations f rhs

instance HasAnnotations DataFamilyInst where
  traverseAnnotations f (DataFamilyInst isNewtype binders instHead kind constructors derivings) =
    DataFamilyInst isNewtype
      <$> traverseAnnotations f binders
      <*> traverseAnnotations f instHead
      <*> traverseAnnotations f kind
      <*> traverseAnnotations f constructors
      <*> traverseAnnotations f derivings

instance HasAnnotations DataDecl where
  traverseAnnotations f (DataDecl pragma dataHead context kind constructors derivings) =
    DataDecl pragma
      <$> traverseAnnotations f dataHead
      <*> traverseAnnotations f context
      <*> traverseAnnotations f kind
      <*> traverseAnnotations f constructors
      <*> traverseAnnotations f derivings

instance HasAnnotations NewtypeDecl where
  traverseAnnotations f (NewtypeDecl pragma newtypeHead context kind constructor derivings) =
    NewtypeDecl pragma
      <$> traverseAnnotations f newtypeHead
      <*> traverseAnnotations f context
      <*> traverseAnnotations f kind
      <*> traverseAnnotations f constructor
      <*> traverseAnnotations f derivings

instance HasAnnotations DataConDecl where
  traverseAnnotations f constructor =
    case constructor of
      DataConAnn ann inner -> DataConAnn <$> f ann <*> traverseAnnotations f inner
      PrefixCon binders context name fields ->
        PrefixCon <$> traverseAnnotations f binders <*> traverseAnnotations f context <*> traverseAnnotations f name <*> traverseAnnotations f fields
      InfixCon binders context lhs name rhs ->
        InfixCon
          <$> traverseAnnotations f binders
          <*> traverseAnnotations f context
          <*> traverseAnnotations f lhs
          <*> traverseAnnotations f name
          <*> traverseAnnotations f rhs
      RecordCon binders context name fields ->
        RecordCon <$> traverseAnnotations f binders <*> traverseAnnotations f context <*> traverseAnnotations f name <*> traverseAnnotations f fields
      GadtCon telescopes context names body ->
        GadtCon <$> traverseAnnotations f telescopes <*> traverseAnnotations f context <*> traverseAnnotations f names <*> traverseAnnotations f body
      TupleCon binders context flavor fields ->
        TupleCon <$> traverseAnnotations f binders <*> traverseAnnotations f context <*> pure flavor <*> traverseAnnotations f fields
      UnboxedSumCon binders context position arity field ->
        UnboxedSumCon <$> traverseAnnotations f binders <*> traverseAnnotations f context <*> pure position <*> pure arity <*> traverseAnnotations f field
      ListCon binders context -> ListCon <$> traverseAnnotations f binders <*> traverseAnnotations f context

instance HasAnnotations GadtBody where
  traverseAnnotations f body =
    case body of
      GadtPrefixBody arguments result -> GadtPrefixBody <$> traverseAnnotations f arguments <*> traverseAnnotations f result
      GadtRecordBody fields result -> GadtRecordBody <$> traverseAnnotations f fields <*> traverseAnnotations f result

instance HasAnnotations BangType where
  traverseAnnotations f (BangType anns pragmas strict lazy ty) =
    BangType <$> traverseAnnotations f anns <*> pure pragmas <*> pure strict <*> pure lazy <*> traverseAnnotations f ty

instance HasAnnotations FieldDecl where
  traverseAnnotations f (FieldDecl anns names multiplicity ty) =
    FieldDecl <$> traverseAnnotations f anns <*> traverseAnnotations f names <*> traverseAnnotations f multiplicity <*> traverseAnnotations f ty

instance HasAnnotations DerivingClause where
  traverseAnnotations f (DerivingClause strategy classes) =
    DerivingClause <$> traverseAnnotations f strategy <*> traverseAnnotations f classes

instance HasAnnotations DerivingStrategy where
  traverseAnnotations f strategy =
    case strategy of
      DerivingStock -> pure DerivingStock
      DerivingNewtype -> pure DerivingNewtype
      DerivingAnyclass -> pure DerivingAnyclass
      DerivingVia ty -> DerivingVia <$> traverseAnnotations f ty

instance HasAnnotations StandaloneDerivingDecl where
  traverseAnnotations f (StandaloneDerivingDecl strategy pragmas warning binders context instHead) =
    StandaloneDerivingDecl
      <$> traverseAnnotations f strategy
      <*> pure pragmas
      <*> pure warning
      <*> traverseAnnotations f binders
      <*> traverseAnnotations f context
      <*> traverseAnnotations f instHead

-- Classes and instances

instance HasAnnotations ClassDecl where
  traverseAnnotations f (ClassDecl context classHead fundeps items) =
    ClassDecl
      <$> traverseAnnotations f context
      <*> traverseAnnotations f classHead
      <*> traverseAnnotations f fundeps
      <*> traverseAnnotations f items

instance HasAnnotations FunctionalDependency where
  traverseAnnotations f fundep =
    (\anns -> fundep {functionalDependencyAnns = anns})
      <$> traverseAnnotations f (functionalDependencyAnns fundep)

instance HasAnnotations ClassDeclItem where
  traverseAnnotations f item =
    case item of
      ClassItemAnn ann inner -> ClassItemAnn <$> f ann <*> traverseAnnotations f inner
      ClassItemTypeSig names ty -> ClassItemTypeSig <$> traverseAnnotations f names <*> traverseAnnotations f ty
      ClassItemDefaultSig name ty -> ClassItemDefaultSig <$> traverseAnnotations f name <*> traverseAnnotations f ty
      ClassItemFixity assoc namespace precedence operators ->
        ClassItemFixity assoc namespace precedence <$> traverseAnnotations f operators
      ClassItemDefault value -> ClassItemDefault <$> traverseAnnotations f value
      ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> traverseAnnotations f familyDecl
      ClassItemDataFamilyDecl familyDecl -> ClassItemDataFamilyDecl <$> traverseAnnotations f familyDecl
      ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> traverseAnnotations f familyInst
      ClassItemPragma pragma -> pure (ClassItemPragma pragma)

instance HasAnnotations InstanceDecl where
  traverseAnnotations f (InstanceDecl pragmas warning binders context instHead items) =
    InstanceDecl pragmas warning
      <$> traverseAnnotations f binders
      <*> traverseAnnotations f context
      <*> traverseAnnotations f instHead
      <*> traverseAnnotations f items

instance HasAnnotations InstanceDeclItem where
  traverseAnnotations f item =
    case item of
      InstanceItemAnn ann inner -> InstanceItemAnn <$> f ann <*> traverseAnnotations f inner
      InstanceItemBind value -> InstanceItemBind <$> traverseAnnotations f value
      InstanceItemTypeSig names ty -> InstanceItemTypeSig <$> traverseAnnotations f names <*> traverseAnnotations f ty
      InstanceItemFixity assoc namespace precedence operators ->
        InstanceItemFixity assoc namespace precedence <$> traverseAnnotations f operators
      InstanceItemTypeFamilyInst familyInst -> InstanceItemTypeFamilyInst <$> traverseAnnotations f familyInst
      InstanceItemDataFamilyInst familyInst -> InstanceItemDataFamilyInst <$> traverseAnnotations f familyInst
      InstanceItemPragma pragma -> pure (InstanceItemPragma pragma)

instance HasAnnotations ForeignDecl where
  traverseAnnotations f decl =
    (\name ty -> decl {foreignName = name, foreignType = ty})
      <$> traverseAnnotations f (foreignName decl)
      <*> traverseAnnotations f (foreignType decl)

-- Expressions

instance HasAnnotations Expr where
  traverseAnnotations f expr =
    case expr of
      EAnn ann inner -> EAnn <$> f ann <*> traverseAnnotations f inner
      EVar name -> EVar <$> traverseAnnotations f name
      EImplicitParam {} -> pure expr
      ETypeSyntax form ty -> ETypeSyntax form <$> traverseAnnotations f ty
      EInt {} -> pure expr
      EFloat {} -> pure expr
      EChar {} -> pure expr
      ECharHash {} -> pure expr
      EString {} -> pure expr
      EStringHash {} -> pure expr
      EOverloadedLabel {} -> pure expr
      EQuasiQuote {} -> pure expr
      EIf condition thenExpr elseExpr ->
        EIf <$> traverseAnnotations f condition <*> traverseAnnotations f thenExpr <*> traverseAnnotations f elseExpr
      EMultiWayIf alternatives -> EMultiWayIf <$> traverseAnnotations f alternatives
      ELambdaPats pats body -> ELambdaPats <$> traverseAnnotations f pats <*> traverseAnnotations f body
      ELambdaCase alternatives -> ELambdaCase <$> traverseAnnotations f alternatives
      ELambdaCases alternatives -> ELambdaCases <$> traverseAnnotations f alternatives
      EInfix lhs name rhs -> EInfix <$> traverseAnnotations f lhs <*> traverseAnnotations f name <*> traverseAnnotations f rhs
      EViewPat lhs rhs -> EViewPat <$> traverseAnnotations f lhs <*> traverseAnnotations f rhs
      ENegate inner -> ENegate <$> traverseAnnotations f inner
      ESectionL inner name -> ESectionL <$> traverseAnnotations f inner <*> traverseAnnotations f name
      ESectionR name inner -> ESectionR <$> traverseAnnotations f name <*> traverseAnnotations f inner
      ELetDecls decls body -> ELetDecls <$> traverseAnnotations f decls <*> traverseAnnotations f body
      ECase scrutinee alternatives -> ECase <$> traverseAnnotations f scrutinee <*> traverseAnnotations f alternatives
      EDo statements flavor -> EDo <$> traverseAnnotations f statements <*> pure flavor
      EListComp body statements -> EListComp <$> traverseAnnotations f body <*> traverseAnnotations f statements
      EListCompParallel body branches -> EListCompParallel <$> traverseAnnotations f body <*> traverseAnnotations f branches
      EArithSeq sequence' -> EArithSeq <$> traverseAnnotations f sequence'
      ERecordCon name fields wildcard -> ERecordCon <$> traverseAnnotations f name <*> traverseAnnotations f fields <*> pure wildcard
      ERecordUpd record fields -> ERecordUpd <$> traverseAnnotations f record <*> traverseAnnotations f fields
      EGetField record field -> EGetField <$> traverseAnnotations f record <*> traverseAnnotations f field
      EGetFieldProjection fields -> EGetFieldProjection <$> traverseAnnotations f fields
      ETypeSig inner ty -> ETypeSig <$> traverseAnnotations f inner <*> traverseAnnotations f ty
      EParen inner -> EParen <$> traverseAnnotations f inner
      EList items -> EList <$> traverseAnnotations f items
      ETuple flavor items -> ETuple flavor <$> traverseAnnotations f items
      EUnboxedSum position arity inner -> EUnboxedSum position arity <$> traverseAnnotations f inner
      ETypeApp function ty -> ETypeApp <$> traverseAnnotations f function <*> traverseAnnotations f ty
      EApp function argument -> EApp <$> traverseAnnotations f function <*> traverseAnnotations f argument
      ETHExpQuote inner -> ETHExpQuote <$> traverseAnnotations f inner
      ETHTypedQuote inner -> ETHTypedQuote <$> traverseAnnotations f inner
      ETHDeclQuote decls -> ETHDeclQuote <$> traverseAnnotations f decls
      ETHTypeQuote ty -> ETHTypeQuote <$> traverseAnnotations f ty
      ETHPatQuote pat -> ETHPatQuote <$> traverseAnnotations f pat
      ETHNameQuote inner -> ETHNameQuote <$> traverseAnnotations f inner
      ETHTypeNameQuote ty -> ETHTypeNameQuote <$> traverseAnnotations f ty
      ETHSplice inner -> ETHSplice <$> traverseAnnotations f inner
      ETHTypedSplice inner -> ETHTypedSplice <$> traverseAnnotations f inner
      EProc pat command -> EProc <$> traverseAnnotations f pat <*> traverseAnnotations f command
      EPragma pragma inner -> EPragma pragma <$> traverseAnnotations f inner

instance (HasAnnotations body) => HasAnnotations (CaseAlt body) where
  traverseAnnotations f (CaseAlt anns pat rhs) =
    CaseAlt <$> traverseAnnotations f anns <*> traverseAnnotations f pat <*> traverseAnnotations f rhs

instance HasAnnotations LambdaCaseAlt where
  traverseAnnotations f (LambdaCaseAlt anns pats rhs) =
    LambdaCaseAlt <$> traverseAnnotations f anns <*> traverseAnnotations f pats <*> traverseAnnotations f rhs

instance (HasAnnotations body) => HasAnnotations (DoStmt body) where
  traverseAnnotations f statement =
    case statement of
      DoAnn ann inner -> DoAnn <$> f ann <*> traverseAnnotations f inner
      DoBind pat body -> DoBind <$> traverseAnnotations f pat <*> traverseAnnotations f body
      DoLetDecls decls -> DoLetDecls <$> traverseAnnotations f decls
      DoExpr body -> DoExpr <$> traverseAnnotations f body
      DoRecStmt statements -> DoRecStmt <$> traverseAnnotations f statements

instance HasAnnotations Cmd where
  traverseAnnotations f command =
    case command of
      CmdAnn ann inner -> CmdAnn <$> f ann <*> traverseAnnotations f inner
      CmdArrApp function appType argument ->
        CmdArrApp <$> traverseAnnotations f function <*> pure appType <*> traverseAnnotations f argument
      CmdInfix lhs name rhs -> CmdInfix <$> traverseAnnotations f lhs <*> traverseAnnotations f name <*> traverseAnnotations f rhs
      CmdDo statements -> CmdDo <$> traverseAnnotations f statements
      CmdIf condition thenCmd elseCmd ->
        CmdIf <$> traverseAnnotations f condition <*> traverseAnnotations f thenCmd <*> traverseAnnotations f elseCmd
      CmdCase scrutinee alternatives -> CmdCase <$> traverseAnnotations f scrutinee <*> traverseAnnotations f alternatives
      CmdLet decls inner -> CmdLet <$> traverseAnnotations f decls <*> traverseAnnotations f inner
      CmdLam pats inner -> CmdLam <$> traverseAnnotations f pats <*> traverseAnnotations f inner
      CmdApp inner argument -> CmdApp <$> traverseAnnotations f inner <*> traverseAnnotations f argument
      CmdPar inner -> CmdPar <$> traverseAnnotations f inner

instance HasAnnotations CompStmt where
  traverseAnnotations f statement =
    case statement of
      CompAnn ann inner -> CompAnn <$> f ann <*> traverseAnnotations f inner
      CompGen pat expr -> CompGen <$> traverseAnnotations f pat <*> traverseAnnotations f expr
      CompGuard expr -> CompGuard <$> traverseAnnotations f expr
      CompLetDecls decls -> CompLetDecls <$> traverseAnnotations f decls
      CompThen expr -> CompThen <$> traverseAnnotations f expr
      CompThenBy function expr -> CompThenBy <$> traverseAnnotations f function <*> traverseAnnotations f expr
      CompGroupUsing function -> CompGroupUsing <$> traverseAnnotations f function
      CompGroupByUsing expr function -> CompGroupByUsing <$> traverseAnnotations f expr <*> traverseAnnotations f function

instance HasAnnotations ArithSeq where
  traverseAnnotations f sequence' =
    case sequence' of
      ArithSeqAnn ann inner -> ArithSeqAnn <$> f ann <*> traverseAnnotations f inner
      ArithSeqFrom from -> ArithSeqFrom <$> traverseAnnotations f from
      ArithSeqFromThen from next -> ArithSeqFromThen <$> traverseAnnotations f from <*> traverseAnnotations f next
      ArithSeqFromTo from to -> ArithSeqFromTo <$> traverseAnnotations f from <*> traverseAnnotations f to
      ArithSeqFromThenTo from next to ->
        ArithSeqFromThenTo <$> traverseAnnotations f from <*> traverseAnnotations f next <*> traverseAnnotations f to
