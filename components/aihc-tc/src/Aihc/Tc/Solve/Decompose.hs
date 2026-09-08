-- | Structural rules for nominal equality.
module Aihc.Tc.Solve.Decompose
  ( decomposeNominalEquality,
  )
where

import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (isTypeFamilyTyCon, unsaturateFamilyApplication)
import Aihc.Tc.Types

-- | Return the equalities that the outer type structure permits.
-- A saturated family application is opaque. Extra arguments can decompose.
-- The caller must preserve evidence and apply these rules to each child.
decomposeNominalEquality :: TcType -> TcType -> TcM (Maybe [(TcType, TcType)])
decomposeNominalEquality rawLeft rawRight = do
  left <- unsaturateFamilyApplication rawLeft
  right <- unsaturateFamilyApplication rawRight
  isFamily <- isTypeFamilyTyCon
  let familyHead (TcTyCon tyCon _) = isFamily tyCon
      familyHead _ = False
  if familyHead left || familyHead right
    then pure Nothing
    else decompose left right
  where
    decompose (TcTyCon leftCon leftArgs) (TcTyCon rightCon rightArgs)
      | leftCon == rightCon,
        length leftArgs == length rightArgs =
          pure (Just (zip leftArgs rightArgs))
    decompose (TcFunTy leftArg leftResult) (TcFunTy rightArg rightResult) =
      pure (Just [(leftArg, rightArg), (leftResult, rightResult)])
    decompose (TcAppTy function argument) (TcFunTy domain range) = do
      arrow <- arrowTyCon
      pure (Just [(function, TcTyCon arrow [domain]), (argument, range)])
    decompose (TcFunTy domain range) (TcAppTy function argument) = do
      arrow <- arrowTyCon
      pure (Just [(TcTyCon arrow [domain], function), (range, argument)])
    decompose (TcAppTy function argument) (TcTyCon tyCon arguments)
      | not (null arguments) =
          pure (Just [(function, TcTyCon tyCon (init arguments)), (argument, last arguments)])
    decompose (TcTyCon tyCon arguments) (TcAppTy function argument)
      | not (null arguments) =
          pure (Just [(TcTyCon tyCon (init arguments), function), (last arguments, argument)])
    decompose (TcAppTy leftFunction leftArgument) (TcAppTy rightFunction rightArgument) =
      pure (Just [(leftFunction, rightFunction), (leftArgument, rightArgument)])
    decompose _ _ = pure Nothing

    arrowTyCon = wiredTyCon tcWiringArrowTyCon (KFun KType (KFun KType KType))
