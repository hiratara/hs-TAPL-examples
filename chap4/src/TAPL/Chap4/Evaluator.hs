{-# LANGUAGE Strict #-}
module TAPL.Chap4.Evaluator
  ( eval
  , isVal
  ) where

import TAPL.Chap4.Types

dummyInfo :: Info
dummyInfo = Info

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t | isNumericVal t = True
        | otherwise = False

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t _) = Just t
eval1 (TmIf _ (TmFalse _) _ t) = Just t
eval1 (TmIf i t1 t2 t3) = do
  t1' <- eval1 t1
  Just (TmIf i t1' t2 t3)
eval1 (TmSucc i t) = do
  t' <- eval1 t
  Just (TmSucc i t')
eval1 (TmPred _ (TmZero _)) = Just (TmZero dummyInfo)
eval1 (TmPred _ (TmSucc _ nv)) | isNumericVal nv = Just nv
eval1 (TmPred i t) = do
  t' <- eval1 t
  Just (TmPred i t')
eval1 (TmIsZero _ (TmZero _)) = Just (TmTrue dummyInfo)
eval1 (TmIsZero _ (TmSucc _ nv)) | isNumericVal nv = Just (TmFalse dummyInfo)
eval1 (TmIsZero f t) = do
  t' <- eval1 t
  Just (TmIsZero f t')
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of
  Just t' -> eval t'
  Nothing -> t
