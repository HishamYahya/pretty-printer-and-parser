module Test_While where

import While
import Test.QuickCheck
import Control.Monad
-- Quickcheck Generators

instance Arbitrary Binop where
  arbitrary = elements [Plus,Minus,Times,Div,Equal,Less,LessEq,And,Or]


gen_exp lp = sized tree where
  ident = elements ["x","y","z","a","b"]
  val = oneof [return 0, return 1, arbitrary]
  leaf = oneof [liftM Const val, liftM Var ident]
  tree 0 = leaf
  tree n = frequency [
      (lp,leaf), (2,liftM Uminus subtree), (5,liftM3 Binop arbitrary subtree subtree)
    ] where
    n'=n `div` 2
    subtree = tree n'

gen_small_exp = gen_exp 10
gen_norm_exp = gen_exp 5

instance Arbitrary Exp where
  arbitrary = gen_norm_exp

instance Arbitrary Com where
  arbitrary = sized tree where
    ident = elements ["x","y","z","a","b"]
    tree 0 = return (Seq [])
    tree n = frequency [
                (3,liftM Seq (do l <- chooseInt (0,n'); vectorOf l subtree)),
                (5,liftM2 Assign ident gen_small_exp),
                (2,liftM3 If gen_small_exp subtree subtree),
                (1,liftM2 While gen_small_exp subtree)]
              where n'=n `div` 2
                    subtree = tree n'








