{-# LANGUAGE OverloadedStrings #-}
import qualified System.IO as IO
import TAPL.Chap4.Parser (parse)

main :: IO ()
main = do
  note (parse "0")
  note (parse "succ 0")
  note (parse "if (iszero (succ 0)) then 0 else (pred (succ 0))")
  return ()
  where
    note = IO.hPrint IO.stderr
