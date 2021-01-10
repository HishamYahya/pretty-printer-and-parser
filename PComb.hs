{-
  Simple parser combinator library, similar to what was presented in lecture

-}
module PComb (
  Parser,
  parse, parseAll,

  item,sat,ident_start, ident_ctd,

  many,some,

  digit,letter,char,string,

  space1,space,token,

  ident,keyword,ctrl,
  nat_number,number,

  sepBy1,sepBy
  ) where

-- Using Applicative infrastructure from Control.Applicative

import Control.Monad(liftM,ap,liftM2,guard)
import Control.Applicative( empty, (<|>), Alternative )
import Data.Char(isDigit,isLetter,isSpace)

data Parser a = Parser { parse :: String -> Maybe (a, String) }

parseAll p s = case parse p s of
                    Just (a,"") -> Just a
                    _ -> Nothing

-- Parsing from a String, primitives
item = Parser item'
  where item' [] = Nothing
        item' (c:cs) = Just (c,cs)

-- Combinators
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = Parser (\s -> Just (x,s))
  (<*>) = ap

instance Monad Parser where
  (>>=) m f = Parser (\s -> case parse m s of
                              Nothing -> Nothing
                              Just (x,s') -> parse (f x) s')

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  (<|>) p q = Parser (\s -> case parse p s of Just r -> Just r; _ -> parse q s)

many p = liftM2 (:) p (many p) <|> return []
some p = liftM2 (:) p (many p)

-- Derived
sat prop = do
  c <- item
  guard (prop c)
  return c

digit = sat isDigit
letter = sat isLetter
char c = sat (==c)
string cs = mapM_ char cs

-- Handling space
space1 = sat isSpace
space = do many space1; return ()
token p = do space; r<-p; space; return r

-- Identifiers
ident_start = letter <|> char '_'
ident_ctd = ident_start <|> digit
ident = token $ liftM2 (:) ident_start (many ident_ctd)
-- keyword: same restrictions as identifier
keyword cs = do r<-ident; guard (r==cs)
-- control, e.g., "==": no space to next keyword required
ctrl cs = do token (string cs)

-- Numbers
nat_number :: Parser Integer
nat_number = token (liftM read $ some digit)

number :: Parser Integer
number = token pn where
  pn = n <|> (do char '-'; r<-n; return (-r))
  n = liftM read $ some digit

-- Lists
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = liftM2 (:) p (many (sep *> p))

sepBy p sep = sepBy1 p sep <|> return []




