{-# LANGUAGE  GADTs #-}

module RexpParser
    (UniOp(..), BinOp(..), StateCore(..),
    Alphabet(..), StState(..), FStateSet(..),
    ImportantStates(..), DeltaCore(..)
    , Transition (..), AllStates (..)
    , Name (..), FAid (..), FA (..),
    RexpExpr (..), parseProgTag, Stmt (..)
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

newtype Name = Nm String deriving Show

data FAtype where
  FaT :: String -> FAtype
  deriving Show

data FAid where
  ID :: FAtype -> Name -> FAid
  deriving Show

newtype Statements = Stmts [Stmt] deriving Show

data Program = P [FA] Statements deriving Show

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
   Decl   :: Type     -> Var       -> Stmt           -- <type> <var>
   Assign :: Type     -> Var       -> RexpExpr  -> Stmt
   --Inc    :: Var      -> RexpExpr  -> Stmt         -- <var> ':=' <expr>
   --Block  :: Prog     -> Stmt                  -- '{' <prog> '}'
   --If     :: RexpExpr -> Stmt      -> Stmt -> Stmt  -- 'if' <expr> 'then' <stmt> 'else' <stmt>
   --Repeat :: RexpExpr -> Stmt      -> Stmt          -- 'repeat' <expr> <stmt>
   --While  :: RexpExpr -> Stmt      -> Stmt          -- 'while' <expr> <stmt>
   --Input  :: Var      -> Stmt                  -- 'input' <var>
   --Output :: RexpExpr -> Stmt                  -- 'output' <expr>
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

{-< Parsing the FA tag >-}

{- Basic parser for common tags <E>stuff</E> -}

parseStartTag :: String -> Parser ()
parseStartTag s = reservedOp "<" *> reserved s *> reservedOp ">"

parseEndTag :: String -> Parser ()
parseEndTag s = endTag *> reserved s *> reservedOp ">"

{- Parsing the alphabet tag then becomes straight forward.-}
parseAlphabet :: Parser Alphabet
parseAlphabet = A <$> (P.try (parseStartTag "alphabet"
                *> (show <$> integer)  <* parseEndTag "alphabet")
                <|> (parseStartTag "alphabet"
                *> identifier <* parseEndTag "alphabet"))

{- To parse the delta tag, we need to first parse the transition tags
   inside it. Each of these is thus considered as a lexical element that
   ends with a simi-colon which allows us to write the whole delta tag.-}

parseTransition :: Parser Transition
parseTransition = Tr <$> (St <$> (parseStartTag "transition"
                 *> reserved "from" *> identifier)) <*> (reserved "with"
                 *> anyChar <* (space <* reserved "to"))
                 <*> (St <$> (identifier <* parseEndTag "transition"))

parseTS :: Parser [Transition]
parseTS = semiSep1 parseTransition

parseDelta :: Parser DeltaCore
parseDelta = Dt <$> (parseStartTag "delta" *> parseTS <* parseEndTag "delta")

{- Parse the whole states tag, its start tag takes two parameters
   starting, as well as final. Starting indicates the starting state,
   while final indicates the set of all final states.-}

parseEquals :: String -> Parser ()
parseEquals s = reserved s *> reservedOp "="

parseStarting :: Parser StState
parseStarting = Start <$> (parseEquals "starting" *> (St <$> identifier))

parseFinal :: Parser FStateSet
parseFinal = Final <$> (parseEquals "final" *>
                           commaSep1 (St <$> identifier))

parseStartStatesTag :: Parser ImportantStates
parseStartStatesTag = Imp <$> (reservedOp "<" *> reserved "states" *> parseStarting)
                          <*> (parseFinal <* reservedOp ">")

parseAllStates :: Parser AllStates
parseAllStates = All <$> parseStartStatesTag
                     <*> braces (commaSep1 $ St <$> identifier) <* parseEndTag "states"

{- Parsing the FA tag is a matter of correctly parsing its starting tag
   which has two parameters type and name, and then just combining the
   parsers above to get the desired outcome.-}

parseFAtype :: Parser FAtype
parseFAtype = FaT <$> (parseEquals "type" *> identifier)

parseFAname :: Parser Name
parseFAname = Nm <$> (parseEquals "name" *> identifier)

parseStFAtag :: Parser FAid
parseStFAtag = ID <$> (reservedOp "<" *> reserved "FA" *> parseFAtype)
                  <*> (parseFAname <* reservedOp ">")

parseFA :: Parser FA
parseFA = FA <$> parseStFAtag <*> parseAlphabet
            <*> parseAllStates <*> parseDelta <* parseEndTag "FA"


{-< Parsing the Statements tag >-}

parseRexpExprAtom :: Parser RexpExpr
parseRexpExprAtom =  Autom .Nm <$> identifier
                 <|> Var <$> identifier <|> parens parseRexpExpr
                 -- <|> parseRexpExpr



parseRexpExpr :: Parser RexpExpr
parseRexpExpr = buildExpressionParser table parseRexpExprAtom
   where
     table = [
            [Prefix (Uni Neg  <$ reservedOp "!")]
          --,  Prefix (Uni Conv <$ reserved "convert")]
          , [Infix (Bin Plus <$ reservedOp "+") AssocRight]
          , [Infix (Bin Times <$ reservedOp "*") AssocRight]]


parseType :: Parser Type
parseType = TyDFA <$ reserved "DFA" <|> TyNFA <$ reserved "NFA"

{- Parsing Statements -}
parseStmt :: Parser Stmt
parseStmt = P.try (Assign <$> parseType <*> (identifier <* reservedOp "=")
                    <*> parseRexpExpr)
         <|>  Decl <$> parseType <*> identifier


parseStmtTag :: Parser Statements
parseStmtTag = Stmts <$> (parseStartTag "statements"
                     *> semiSep1 parseStmt <* parseEndTag "statements")



{- Add Prog Parser -}
parseProgTag :: Parser Program
parseProgTag = P <$> (parseStartTag "rexp" *> commaSep1 parseFA)
                 <*> (parseStmtTag <* parseEndTag "rexp")
