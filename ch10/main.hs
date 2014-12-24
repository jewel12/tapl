import Eval
import Parser (runParser)
import Type

import Control.Applicative((<$>))

runTypeof :: String -> Either String Ty
runTypeof input = do
  term <- runParser input
  typeof [] term

parseAndEval :: String -> Either String Term
parseAndEval input = do
  term <- runParser input
  typeof [] term
  return $ eval [] term

run :: String -> String
run input = case (parseAndEval input) of
              Right term -> printtm [] term
              Left e -> e

main = mapM_ ( print . run ) [
        "^x:Bool. if x then false else true",
        "^x:Bool. ^y:Bool. if x false then (^z:Bool. x) else y" -- arrow type expected
        ]
