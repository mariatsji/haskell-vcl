{-# language
  DeriveFunctor,
  LambdaCase,
  QuasiQuotes,
  OverloadedStrings
#-}

module Main where

import Control.Applicative ((<|>), many, optional, some)
import Data.Attoparsec.Expr
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified ApiAccessAdmin
import qualified ApiAdmin
import NeatInterpolation (text)


data VCL a = VCL { unVCL :: [TopStatement a] } deriving (Eq, Show, Functor)

data Statement a =
  IfStatement a [Statement a] (Maybe [Statement a]) |
  FCall SubName [a] |
  Return SubName [a] |
  Set Variable a
  deriving (Eq, Show, Functor)

type Variable = Text
type SubName = Text

data Expression =
  VarLit Text |
  Bin BinOp Expression Expression |
  Not Expression |
  StringLit Text |
  NumLit Integer
  deriving Show

data SetExpr =
  VarLitS Text |
  Multi BinOp (Set SetExpr) |
  NotS SetExpr |
  StringLitS Text |
  NumLitS Integer
  deriving (Eq, Ord, Show)

subItems :: BinOp -> SetExpr -> Set SetExpr
subItems op = \case
  Multi op' set | op == op' -> foldMap (subItems op) set
  a -> Set.singleton a

transform :: Expression -> SetExpr
transform = \case
  VarLit a -> VarLitS a
  Not a -> NotS $ transform a
  StringLit a -> StringLitS a
  NumLit a -> NumLitS a
  Bin op a b -> Multi op $ Set.union (subItems op $ transform a) (subItems op $ transform b)
  --Bin And (Bin And a) b -> case (transform a, transform b) of
  --                           (Multi And s, )

data TopStatement a = SubRoutine Text [Statement a] deriving (Eq, Show, Functor)

data BinOp = Add | And | Or | Match | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual deriving (Eq, Ord, Show)

instance Eq Expression where
  Bin Or a1 b1 == Bin Or a2 b2 = a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2
  Bin And a1 b1 == Bin And a2 b2 = a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2
  Bin bop1 a1 b1 == Bin bop2 a2 b2 = bop1 == bop2 && a1 == a2 && b1 == b2
  VarLit a1 == VarLit a2 = a1 == a2
  Not a1 == Not a2 = a1 == a2
  StringLit a1 == StringLit a2 = a1 == a2
  NumLit a1 == NumLit a2 = a1 == a2
  _ == _ = False
  

skipSpace :: Parser ()
skipSpace = A.skipSpace *> A.char '#' *> A.manyTill A.anyChar A.endOfLine *> skipSpace <|>
  A.skipSpace <?> "no skipSpace"

nameParser :: Parser Text
nameParser = ((skipSpace *>) . fmap T.pack $ (:) <$> (A.letter <|> A.char '_') <*> many (A.letter <|> A.char '_' <|> A.digit <|> A.char '.' <|> A.char '-'))
  <?> "no nameParser"
  

topParser :: Parser (VCL Expression) 
topParser = (fmap VCL . some $ SubRoutine <$> (skipSpace *> A.string "sub" *> nameParser) <*> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}'))
  <?> "no topParse"

paramParser :: Parser [Expression]
paramParser = skipSpace *> A.char '(' *> A.sepBy exprParser (skipSpace *> A.char ',' <* skipSpace) <* skipSpace <* A.char ')' <?> "no paramParser"

statementParser :: Parser (Statement Expression)
statementParser = ((skipSpace *>) $
  IfStatement <$> (A.string "if" *> skipSpace *> A.char '(' *> exprParser <* skipSpace <* A.char ')') <*> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}' <|> pure <$> statementParser) <*> optional (skipSpace *> A.string "else" *> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}' <|> pure <$> statementParser)) <|>
  FCall <$> nameParser <*> paramParser <* skipSpace <* A.char ';' <|>
  Return <$> (A.string "return" *> skipSpace *> A.char '(' *> nameParser) <*> paramParser <* skipSpace <* A.char ')' <* skipSpace <* A.char ';' <|>
  Set <$> (A.string "set" *> nameParser) <*> (skipSpace *> A.char '=' *> skipSpace *> exprParser) <* skipSpace <* A.char ';')
  <?> "no statementParser"

stringParser :: Parser Text
stringParser = A.char '"' *> A.takeWhile (/= '"') <* A.char '"' -- TODO
  <?> "no stringParser"

opTable :: OperatorTable Text Expression
opTable = [
    [Prefix (skipSpace *> (Not <$ A.char '!'))]
  , [Infix (skipSpace *> (Bin Add <$ A.char '+')) AssocLeft]
  , [Infix (skipSpace *> (Bin Match <$ A.char '~')) AssocLeft]
  , [Infix (skipSpace *> (Bin Equal <$ A.string "==")) AssocLeft]
  , [Infix (skipSpace *> (Bin NotEqual <$ A.string "!=")) AssocLeft]
  , [Infix (skipSpace *> (Bin Less <$ A.char '<')) AssocLeft]
  , [Infix (skipSpace *> (Bin Greater <$ A.char '>')) AssocLeft]
  , [Infix (skipSpace *> (Bin LessEqual <$ A.string "<=")) AssocLeft]
  , [Infix (skipSpace *> (Bin GreaterEqual <$ A.string ">=")) AssocLeft]
  , [Infix (skipSpace *> (Bin And <$ A.string "&&")) AssocLeft]
  , [Infix (skipSpace *> (Bin Or <$ A.string "||")) AssocLeft]
  ]

termParser :: Parser Expression
termParser = ((skipSpace *>) $
  VarLit <$> nameParser <|>
  StringLit <$> stringParser <|>
  NumLit <$> A.decimal  <|>
  A.char '(' *> skipSpace *> exprParser <* skipSpace <* A.char ')') <?>
  "no termParser"

exprParser :: Parser Expression
exprParser = buildExpressionParser opTable termParser <?> "no exprParser"

supertest :: IO ()
supertest = do
  let oldParse = A.parseOnly topParser ApiAdmin.small
      newParse = A.parseOnly topParser ApiAccessAdmin.small
  print oldParse
  print newParse
  print $ oldParse == newParse

main' :: IO ()
main' = do
  let first = A.parseOnly exprParser [text|
    foo ~ ".*" && bar == 1
  |]
  let second = A.parseOnly exprParser [text|
    bar == 1 && foo ~ ".*"
  |]
  print first
  print second
  print $ first == second

a = [text|
  sub policy_recv {
    if(((req.url ~ "^/iad[/]?$" || req.url ~ "^/iad/ad/\d+/?$" || req.url ~ "^/iad/ad/job-.*" || req.url ~ "^/iad/ad/model/job-.*" || req.url ~ "^/iad/image/size$" || req.url ~ "^/iad/search/job-.*") && req.method ~ "^(GET|HEAD)$")) {
      # NOOP
    }
  }
  |]

b = [text|
  sub policy_recv {
    if(((req.url ~ "^/iad/search/job-.*"
      || req.url ~ "^/iad/ad/model/job-.*"
      || req.url ~ "^/iad/ad/job-.*"
      || req.url ~ "^/iad[/]?$"
      || req.url ~ "^/iad/image/size$"
      || req.url ~ "^/iad/ad/\d+/?$") && req.method ~ "^(GET|HEAD)$")) {
      # NOOP
    }}     
  |]

main :: IO ()
main = do
  let a' = fmap transform <$> A.parseOnly topParser ApiAdmin.vcl
  let b' = fmap transform <$> A.parseOnly topParser ApiAccessAdmin.vcl
  print a'
  print b'
  TIO.writeFile "a.txt" $ (T.pack . show $ a')
  TIO.writeFile "b.txt" $ (T.pack . show $ b')
  print $ a' == b'
