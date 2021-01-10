module Test_Pretty where
import While
import Pretty
import Parse
import Test_While
import Test_Pretty_Cases
import Test_Generic
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import System.IO
import Text.Regex.XMLSchema.Generic (tokenize,tokenizeExt)

{-
  Test cases for pretty printer. There are a fixed number of test cases.
  Each test case is a pair of a string and an AST.
  The test case checks that your pretty printer prints the `same` string.
  Same here means same up to spaces.
  For commands, we actually test two categories:
    same up to spaces,
    same up to spaces AND same indentation (spaces at start of line)


  Note: the actual test data is in the file Test_Pretty_Cases.hs

-}

-- the following function returns the failed test cases
failed_tcs norm pretty tcs = mapMaybe check tcs where
  check (s,a) | s `eqq` s' = Nothing
              | otherwise = Just (norm s,norm s',a)
              where
                s' = pretty a
                eqq a b = norm a == norm b


test1_data = ("Simple expressions",2, failed_tcs norm1 prettyExp test_simple_expr_pairs)
test2_data = ("Random expressions",1, failed_tcs norm1 prettyExp (test_small_expr_pairs_random ++ test_expr_pairs_random))
test3_data = ("Random commands (no indentation)",1, failed_tcs norm1 prettyCom test_com_pairs_random)
test4_data = ("Random commands (with indentation)",1, failed_tcs norm2 prettyCom test_com_pairs_random)

do_test _ (name,m,[]) = printSuccess name m
do_test vrb (name,_,xs) =
  do
    r <- printFailure name 0
    mapM_ explain xs
    return r

  where
    explain _ | not vrb = return ()
    explain (s,s',a) = putStrLn ("AST\n"++show a ++"\n was printed as" ++ pr s' ++ "but expected" ++ pr s)

    pr s = "\n*******************\n" ++ s ++ "\n*******************\n"


test1 vrb = do_test vrb test1_data
test2 vrb = do_test vrb test2_data
test3 vrb = do_test vrb test3_data
test4 vrb = do_test vrb test4_data

main vrb = do
  r1 <- test1 vrb
  r2 <- test2 vrb
  r3 <- test3 vrb
  r4 <- test4 vrb
  return (max 0 (r1+r2+r3+r4))


{- Auxiliary functions -}
norm_line_indent f xs = spaces ++ f xs' where
  spaces = takeWhile (==' ') xs
  xs' = dropWhile (==' ') xs

norm_tokenize = tokenizeExt "-[0-9]+|[A-Z_a-z0-9]+|<=|==|[<={}+*/;()-]|&&|[|][|]"
norm_tks = unwords . norm_tokenize

gen_norm nl = unlines . map nl . filter (not.null) . lines

-- Normalize spacing in between tokens
norm1 = norm_tks . map spc where
  spc c | isSpace c = ' '
        | otherwise = c

-- Normalize spacing in between tokens, keep indentation
norm2 = gen_norm (norm_line_indent norm_tks)

t [] = (Const 0, "", "")
t (x:xs) = if fst x == prettyExp (snd x) then t xs else (snd x, fst x, prettyExp (snd x))