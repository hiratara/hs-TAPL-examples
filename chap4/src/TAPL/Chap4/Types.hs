{-# LANGUAGE Strict #-}
module TAPL.Chap4.Types
  ( Term(..)
  , Info(..)
  ) where

data Info = Info
  deriving (Show, Eq)

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
  deriving (Show, Eq)
