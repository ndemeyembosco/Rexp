module Parsing
  ( -- * Lexing

    TokenParser, makeTokenParser, emptyDef, GenLanguageDef(..)

  , getIdentifier, getReserved, getReservedOp
  , getNatural, getInteger, getSymbol
  , getWhiteSpace, getParens, getFloat
  , getNaturalOrFloat, getSemiSep1, getCommaSep1, getBraces

    -- * Parsing

  , Parser, parse, parseFile, parseSome
  , module Text.Parsec.Expr
  , module Text.Parsec.Combinator
  , module Text.Parsec.Char
  )
  where

import           Prelude                hiding ((<$), (<$>), (<*>))
import qualified Prelude

import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr       hiding (Operator)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       (Consumed (..), Reply (..), State (..),
                                         runParsecT)
import           Text.Parsec.String     (Parser, parseFromFile)
import           Text.Parsec.Token

import qualified Control.Applicative    as A
import           Data.Functor.Identity

------------------------------------------------------------
-- Lexing
------------------------------------------------------------

getIdentifier = identifier
getReserved   = reserved
getReservedOp = reservedOp
getNatural    = natural
getInteger    = integer
getSymbol     = symbol
getWhiteSpace = whiteSpace
getParens     = parens
getBraces     = braces
getFloat      = float
getNaturalOrFloat = naturalOrFloat
getSemiSep1          = semiSep1
getCommaSep1         = commaSep1

-- For more, see http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Token.html

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse p ""

parseFile :: Parser a -> FilePath -> IO (Either P.ParseError a)
parseFile = parseFromFile

parseSome :: Parser a -> String -> Either P.ParseError (a, String)
parseSome p s =
  case runIdentity . getReply . runIdentity $ runParsecT p (State s (initialPos "") ()) of
    Ok a (State rest _ _) _ -> Right (a, rest)
    Error err               -> Left err
  where
    getReply (Consumed r) = r
    getReply (Empty    r) = r

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = (A.<|>)

infixl 4 <$>
(<$>) :: (a -> b) -> Parser a -> Parser b
(<$>) = fmap

infixl 4 <*>
(<*>) :: Parser (a -> b) -> Parser a -> Parser b
(<*>) = (Prelude.<*>)

infixl 4 <$
(<$) :: a -> Parser b -> Parser a
(<$) = (Prelude.<$)
