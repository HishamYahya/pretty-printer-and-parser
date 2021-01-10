module Test_Parse where

import While
import Test_While
import Test_Pretty_Cases
import Test_Pretty (norm1, norm_tokenize)
import Parse (parseCom,parseExp)
import Pretty (prettyCom,prettyExp)
import Test_Generic
import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Char


{-
  Let's start with commands that should not parse
-}

no_parse_exp_list = [
  -- Make sure operators cannot be separated by space
  "a < = b",
  "a = = b",
  "a & & b",
  "a | | b",

  -- Unary minus rule
  "--42",
  "--x",
  -- invalid identifier
  "0x"
  ]

no_parse_com_list = [
  -- Make sure keywords must be separated by space
  "if b thenx=1 else x=2",
  "ifb then x=1 else x=2",
  "if b then x=1 elsex=2",
  "whilex do x=0",
  "while x dox=0"
  ]


test_no_parse parser s =
  case parser s of
       Just a -> ["String '"++s++"' should not parse, but was parsed to: " ++ show a]
       Nothing -> []


no_parse_tests =
         (concat $ map (test_no_parse parseExp) no_parse_exp_list)
      ++ (concat $ map (test_no_parse parseCom) no_parse_com_list)

gen_list_test name m [] vrb = printSuccess name m
gen_list_test name m xs vrb = do
                   r <- printFailure name m
                   if vrb then putStrLn $ unlines xs else return ()
                   return r

test1 = gen_list_test "No parse test" (-1) no_parse_tests


{-
  The next set of tests checks the parser for a few simple expressions and commands.
  The various norm_xxx functions will change the spacing a bit, to ensure the parser correctly handles spacing.
-}

check_parse norm parser (s,a) | r == Just a = []
                              | otherwise = ["'"++norm s++"' should parse as "++show a ++ " but parsed as " ++ show r]
  where r = parser (norm s)

simple_parse_exp_tests norm = (concat $ map (check_parse norm parseExp) test_simple_expr_pairs)
simple_parse_com_tests norm = (concat $ map (check_parse norm parseCom) test_simple_com_pairs)

norm_space_head_tail xs = " \n \n " ++ xs ++ "\n\n   \n"
norm_many_space = intercalate "   " . norm_tokenize

norm_drop_space = dr . norm1 where
  dr (' ':c:xs) | isPunctuation c && c/='_' = dr (c:xs)
                | otherwise = ' ' : dr (c:xs)
  dr (c:xs) = c:dr xs
  dr [] = []


test2 = gen_list_test "parse simple exp" 2 (concat $ map simple_parse_exp_tests [id,norm1,norm_space_head_tail,norm_many_space,norm_drop_space])
test3 = gen_list_test "parse simple com" 1 (concat $ map simple_parse_com_tests [id,norm1,norm_space_head_tail,norm_many_space,norm_drop_space])

{-
  The ultimate test: parsing pretty-printed commands
-}
prop_parse_pretty_inv parse pretty a = (fromJust.parse) (pretty a) == a

doQcTest name m prop = do
  s <- isSuccess <$> (quickCheckResult (withMaxSuccess 100 prop))
  printResult name m s

test4 = doQcTest "parse-pretty-inv (exp)" 1 (prop_parse_pretty_inv parseExp prettyExp)
test5 = doQcTest "parse-pretty-inv (com)" 1 (prop_parse_pretty_inv parseCom prettyCom)

main vrb = do
  r <- sum <$> sequence [test1 vrb, test2 vrb, test3 vrb, test4, test5]
  return $ max r 0


t = parseCom . prettyCom





