{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
module TAPL.Chap4.Parser
  ( parse
  ) where

import qualified Data.ByteString as BS
import           Data.Void (Void)
import qualified Data.Set as Set
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Char.Lexer as L

import TAPL.Chap4.Types

type Parser = M.Parsec Void BS.ByteString

parse :: BS.ByteString -> Term
parse source = case M.parse term "" source of
  Right x -> x
  Left e -> error (show e)

sc :: Parser ()
sc = L.space M.space1 lineCmnt blockCmnt
  where
    lineCmnt = nop
    blockCmnt = nop
    nop = M.failure Nothing (Set.empty)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: BS.ByteString -> Parser BS.ByteString
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

rword :: BS.ByteString -> Parser ()
rword w = (lexeme . M.try) (M.string w *> M.notFollowedBy M.alphaNumChar)

term :: Parser Term
term = trueTerm
 M.<|> falseTerm
 M.<|> ifTerm
 M.<|> zeroTerm
 M.<|> succTerm
 M.<|> predTerm
 M.<|> isZeroTerm
 M.<|> parens term

trueTerm :: Parser Term
trueTerm = do
  rword "true"
  return (TmTrue Info)

falseTerm :: Parser Term
falseTerm = do
  rword "false"
  return (TmFalse Info)

ifTerm :: Parser Term
ifTerm = do
  rword "if"
  cond <- term
  rword "then"
  stmt1 <- term
  rword "else"
  stmt2 <- term
  return (TmIf Info cond stmt1 stmt2)

zeroTerm :: Parser Term
zeroTerm = do
  rword "0"
  return (TmZero Info)

succTerm :: Parser Term
succTerm = do
  rword "succ"
  term1 <- term
  return (TmSucc Info term1)

predTerm :: Parser Term
predTerm = do
  rword "pred"
  term1 <- term
  return (TmPred Info term1)

isZeroTerm :: Parser Term
isZeroTerm = do
  rword "iszero"
  term1 <- term
  return (TmIsZero Info term1)
