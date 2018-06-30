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
parse source = case M.parse termParser "" source of
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

termParser :: Parser Term
termParser = M.between sc M.eof term

oneTerm :: Parser Term
oneTerm = trueTerm
 M.<|> falseTerm
 M.<|> zeroTerm
 M.<|> parens term

term :: Parser Term
term = ifTerm
 M.<|> succTerm
 M.<|> predTerm
 M.<|> isZeroTerm
 M.<|> oneTerm

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
  cond <- oneTerm
  rword "then"
  stmt1 <- oneTerm
  rword "else"
  stmt2 <- oneTerm
  return (TmIf Info cond stmt1 stmt2)

zeroTerm :: Parser Term
zeroTerm = do
  rword "0"
  return (TmZero Info)

succTerm :: Parser Term
succTerm = do
  rword "succ"
  term1 <- oneTerm
  return (TmSucc Info term1)

predTerm :: Parser Term
predTerm = do
  rword "pred"
  term1 <- oneTerm
  return (TmPred Info term1)

isZeroTerm :: Parser Term
isZeroTerm = do
  rword "iszero"
  term1 <- oneTerm
  return (TmIsZero Info term1)
