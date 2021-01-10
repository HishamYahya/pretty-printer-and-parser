module Main where
import qualified Test_Pretty
import qualified Test_Parse
import Test_Generic
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let vrb = not (null args) && args == ["verbose"]

  r1 <- Test_Pretty.main vrb
  r2 <- Test_Parse.main vrb

  printTotalScore (r1+r2)

  return ()

