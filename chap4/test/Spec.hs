{-# LANGUAGE OverloadedStrings #-}
import qualified System.IO as IO
import TAPL.Chap4.Parser (parse)
import TAPL.Chap4.Evaluator (eval, isVal)

main :: IO ()
main = do
  note (parse "0")
  note (parse "succ 0")
  -- note (parse "pred succ 0")
  note (parse "if (iszero (succ 0)) then 0 else (pred (succ 0))")

  let term = parse "if (iszero (succ 0)) then 0 else (succ (pred (succ 0)))"
      term' = eval term
  note term'
  note (isVal term')

  let term2 = parse "succ (iszero (pred (succ (pred (succ 0)))))"
      term2' = eval term2
  note term2'
  note (isVal term2')

  return ()
  where
    note :: Show a => a -> IO ()
    note = IO.hPrint IO.stderr
