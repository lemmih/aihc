-- | Collect all type constructors that an interface stores or references.
module Aihc.Cli.InterfaceTyCons
  ( interfaceTyCons,
    tyConInfoTyCons,
    dataTypeInfoTyCons,
    classInfoTyCons,
    typeSchemeTyCons,
    typeTyCons,
  )
where

import Aihc.Tc
import Aihc.Tc.Annotations (TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..))
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

interfaceTyCons :: TcInterface -> Set.Set TyCon
interfaceTyCons interface =
  Set.unions
    [ Set.unions (map (typeSchemeTyCons . snd) (tcInterfaceTerms interface)),
      Set.unions (map tyConInfoTyCons (tcInterfaceTyCons interface)),
      Set.unions (map dataTypeInfoTyCons (tcInterfaceDataTypes interface)),
      Set.unions (map classInfoTyCons (tcInterfaceClasses interface)),
      Set.unions (map instanceInfoTyCons (tcInterfaceInstances interface)),
      Set.unions (map dataFamilyInstanceInfoTyCons (tcInterfaceDataFamilyInstances interface)),
      Set.unions (map typeFamilyInstanceInfoTyCons (tcInterfaceTypeFamilyInstances interface)),
      Set.unions (map patSynInfoTyCons (tcInterfacePatSyns interface)),
      Set.unions (map (foreignImportInfoTyCons . snd) (tcInterfaceForeignImports interface))
    ]

tyConInfoTyCons :: TyConInfo -> Set.Set TyCon
tyConInfoTyCons info =
  Set.insert (tciTyCon info) $
    typeSchemeTyCons (tciKindScheme info)
      <> maybe mempty typeSynonymInfoTyCons (tciTypeSynonym info)

typeSynonymInfoTyCons :: TypeSynonymInfo -> Set.Set TyCon
typeSynonymInfoTyCons info =
  Set.unions (map tyVarTyCons (tsiParams info))
    <> maybe mempty typeTyCons (tsiBody info)

dataTypeInfoTyCons :: DataTypeInfo -> Set.Set TyCon
dataTypeInfoTyCons info =
  Set.insert (dtiTyCon info) $
    Set.unions (map tyVarTyCons (dtiTyVars info))
      <> typeTyCons (dtiResultKind info)
      <> Set.unions (map dataConInfoTyCons (dtiConstructors info))

dataConInfoTyCons :: DataConInfo -> Set.Set TyCon
dataConInfoTyCons info =
  Set.unions (map tyVarTyCons (dciUnivTyVars info <> dciExTyVars info))
    <> Set.unions (map predTyCons (dciTheta info))
    <> Set.unions (map (typeTyCons . dcfiType) (dciFields info))
    <> typeTyCons (dciResTy info)

classInfoTyCons :: ClassInfo -> Set.Set TyCon
classInfoTyCons info =
  Set.insert (ciTyCon info) $
    Set.unions (map tyVarTyCons (ciKindTyVars info <> ciTyVars info))
      <> Set.unions (map typeTyCons (ciSuperClassTypes info))
      <> Set.unions (map (typeSchemeTyCons . snd) (ciMethods info <> ciDefaultSignatures info))
      <> Set.fromList (map atiTyCon (ciAssociatedTypes info))
      <> Set.unions (map typeFamilyInstanceInfoTyCons (mapMaybe atiDefault (ciAssociatedTypes info)))

instanceInfoTyCons :: InstanceInfo -> Set.Set TyCon
instanceInfoTyCons info =
  typeTyCons (iiDictType info)
    <> Set.unions (map tyVarTyCons (iiTyVars info))
    <> Set.unions (map predTyCons (iiContext info))
    <> Set.unions (map typeTyCons (iiHead info))

dataFamilyInstanceInfoTyCons :: DataFamilyInstanceInfo -> Set.Set TyCon
dataFamilyInstanceInfoTyCons info =
  Set.insert (dfiiRepresentationTyCon info) $
    typeTyCons (dfiiFamilyType info)
      <> Set.unions (map tyVarTyCons (dfiiTyVars info))

typeFamilyInstanceInfoTyCons :: TypeFamilyInstanceInfo -> Set.Set TyCon
typeFamilyInstanceInfoTyCons info =
  Set.unions (map tyVarTyCons (tfiiTyVars info))
    <> typeTyCons (tfiiLeft info)
    <> typeTyCons (tfiiRight info)

typeSchemeTyCons :: TypeScheme -> Set.Set TyCon
typeSchemeTyCons (ForAll variables predicates body) =
  Set.unions (map tyVarTyCons variables)
    <> Set.unions (map predTyCons predicates)
    <> typeTyCons body

tyVarTyCons :: TyVarId -> Set.Set TyCon
tyVarTyCons = typeTyCons . tvKind

predTyCons :: Pred -> Set.Set TyCon
predTyCons predicate = case predicate of
  ClassPred tyCon arguments -> Set.insert tyCon (Set.unions (map typeTyCons arguments))
  EqPred left right -> typeTyCons left <> typeTyCons right
  IParamPred _ payload -> typeTyCons payload
  QuantifiedPred variables antecedents consequent ->
    Set.unions (map tyVarTyCons variables)
      <> Set.unions (map predTyCons antecedents)
      <> predTyCons consequent

typeTyCons :: TcType -> Set.Set TyCon
typeTyCons ty = case ty of
  TcTyVar variable -> tyVarTyCons variable
  TcMetaTv {} -> mempty
  TcArrowTy -> mempty
  TcTyCon tyCon arguments -> Set.insert tyCon (Set.unions (map typeTyCons arguments))
  TcFunTy argument result -> typeTyCons argument <> typeTyCons result
  TcForAllTy variable body -> tyVarTyCons variable <> typeTyCons body
  TcQualTy predicates body -> Set.unions (map predTyCons predicates) <> typeTyCons body
  TcAppTy function argument -> typeTyCons function <> typeTyCons argument

patSynInfoTyCons :: PatSynInfo -> Set.Set TyCon
patSynInfoTyCons info =
  typeSchemeTyCons (psiScheme info)
    <> Set.unions (map predTyCons (psiReqTheta info <> psiProvTheta info))

foreignImportInfoTyCons :: TcForeignImportInfo -> Set.Set TyCon
foreignImportInfoTyCons info = case info of
  TcForeignPrimImport -> mempty
  TcForeignCCallImport _ plan ->
    Set.unions (map marshalTyCons (tcForeignArguments plan <> [tcForeignResult plan]))
  where
    marshalTyCons marshal = typeTyCons (tcForeignSourceType marshal) <> typeTyCons (tcForeignPrimitiveType marshal)
