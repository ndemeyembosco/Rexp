{-# LANGUAGE  GADTs #-}

module RexpParser
    (UniOp(..), BinOp(..), StateCore(..),
    Alphabet(..), StState(..), FStateSet(..),
    ImportantStates(..), DeltaCore(..)
    , Transition (..), AllStates (..)
    , Name (..), FAid (..), FA (..),
    RexpExpr (..), parseProgTag, parseFAid, parseAlphabet
    , Stmt (..), parseAllStates, parseDeltaCore, parseTransition
    , reserved, reservedOp, whiteSpace
    , Program (..) , Statements (..)
    ) where

import Rexp
import Parsing
import Control.Applicative
import qualified Text.Parsec as P



{- Grammar for Rexp-}

-- Contructing the Automaton.

newtype Alphabet     = A String deriving (Show, Eq)
newtype StateCore    = St String deriving (Show, Eq, Ord)
newtype StState      = Start StateCore deriving Show
newtype FStateSet    = Final [StateCore] deriving Show
data ImportantStates = Imp StState FStateSet deriving Show

data AllStates where
  All :: ImportantStates -> [StateCore] -> AllStates
  deriving Show

data Transition where
  Tr  ::  StateCore -> Char -> StateCore -> Transition
  deriving Show

newtype DeltaCore = Dt [Transition] deriving Show

data FA where
  FA :: FAid -> Alphabet -> AllStates -> DeltaCore -> FA
  deriving Show

type Name = String

data FAType = DFA | NFA
   deriving (Show)

data FAid where
  ID :: FAType -> Name -> FAid
  deriving Show

newtype Statements = Stmts [Stmt] deriving Show

type Program = FA

-- Providing statements to manipulate the Automaton.
data RexpExpr where
  Autom :: Name -> RexpExpr
  Bin   :: BinOp -> RexpExpr -> RexpExpr -> RexpExpr
  Uni   :: UniOp -> RexpExpr -> RexpExpr
  Var   :: String -> RexpExpr
  deriving Show

data BinOp where
  Plus  :: BinOp  -- to be interpreted as Union
  Times :: BinOp  -- to be interpreted as Intersection
  deriving Show

data UniOp where
  Neg  :: UniOp    -- Negate
  --Conv :: UniOp    -- Convert FA from one form to another.
  deriving Show


type Var = String

type Prog = [Stmt]

data Type where
  TyDFA  :: Type
  TyNFA  :: Type
 deriving (Show, Eq)

data Stmt where
   Decl   :: Type     -> Var       -> Stmt
   Assign :: Type     -> Var       -> RexpExpr  -> Stmt
  deriving Show



{- Setting up atomic elements of the parser. -}

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens     = getParens lexer

braces :: Parser a -> Parser a
braces = getBraces lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = getCommaSep1 lexer

charLiteral :: Parser Char
charLiteral = getCharLiteral lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = getSemiSep1 lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

integer :: Parser Integer
integer    = getInteger lexer

symbol :: Parser String
symbol = getSymbol lexer ""

endTag :: Parser String
endTag = getSymbol lexer "</"

identifier :: Parser String
identifier = getIdentifier lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseAssign :: String -> Parser ()
parseAssign s = reserved s *> reservedOp "="

parseAlphabet :: Parser Alphabet
parseAlphabet = A <$> (parseAssign "alphabet" *> (identifier <|> (show <$> integer)))

parseStateCore :: Parser StateCore
parseStateCore = St <$> identifier

parseStState :: Parser StState
parseStState = Start <$> (parseAssign "initial" *> parseStateCore)

parseFinal :: Parser FStateSet
parseFinal = Final <$> (parseAssign "final" *> parens (commaSep1 parseStateCore) <* reserved ";")

parseRemStates :: Parser [StateCore]
parseRemStates = parseAssign "all" *> parens (commaSep1 parseStateCore)

parseImportantStates :: Parser ImportantStates
parseImportantStates = Imp <$> (parseStState <* reserved ";") <*> parseFinal

parseAllStates :: Parser AllStates
parseAllStates = All <$> (parseAssign "states" *> reservedOp "{" *> parseImportantStates) <*> (parseRemStates <* reservedOp "}")


parseTransition :: Parser Transition
parseTransition = Tr <$> (parseStateCore <* reservedOp "#") <*> (anyChar <* (whiteSpace <* reservedOp "=")) <*> parseStateCore

parseDeltaCore :: Parser DeltaCore
parseDeltaCore = Dt <$> (parseAssign "delta" *> reserved "{" *> semiSep1 parseTransition) <* reserved "}"

parseFAtype :: Parser FAType
parseFAtype = handleType <$> identifier
     where
       handleType "dfa" = DFA
       handleType "nfa" = NFA

parseFAid :: Parser FAid
parseFAid = ID <$> parseFAtype <*> identifier

parseFA :: Parser FA
parseFA = FA <$> (parseFAid <* (reservedOp "=" <* reservedOp "{"))
             <*> (parseAlphabet)
             <*> parseAllStates
             <*> parseDeltaCore

parseProgTag :: Parser Program
parseProgTag = parseFA
