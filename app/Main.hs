{-# language
  QuasiQuotes,
  OverloadedStrings
#-}

module Main where

import Control.Applicative ((<|>), many, optional, some)
import Data.Attoparsec.Expr
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (text)


data VCL = VCL [TopStatement] deriving Show

data Statement =
  IfStatement Expression [Statement] (Maybe [Statement]) |
  FCall SubName [Expression] |
  Return SubName [Expression] |
  Set Variable Expression
  deriving Show

type Variable = Text
type SubName = Text

data Expression =
  VarLit Text |
  Bin BinOp Expression Expression |
  Compare EqOp Expression Expression |
  Not Expression |
  Match Variable Text |
  Assignment Variable Expression |
  StringLit Text |
  NumLit Integer
  deriving Show

data EqOp = EqualOp | NotEqualOp | GreaterOp | GreaterEqualOp | LessOp | LessEqualOp deriving Show

data TopStatement = SubRoutine Text [Statement] deriving Show

data BinOp = Plus | And | Or deriving Show

nameParser :: Parser Text
nameParser = (A.skipSpace *>) . fmap T.pack $ (:) <$> (A.letter <|> A.char '_') <*> many (A.letter <|> A.char '_' <|> A.digit <|> A.char '.' <|> A.char '-')

topParser :: Parser VCL
topParser = fmap VCL . some $ SubRoutine <$> (A.string "sub" *> nameParser) <*> (A.skipSpace *> A.char '{' *> many statementParser <* A.skipSpace <* A.char '}')

paramParser :: Parser [Expression]
paramParser = A.skipSpace *> A.char '(' *> A.sepBy exprParser (A.skipSpace *> A.char ',' <* A.skipSpace) <* A.skipSpace <* A.char ')'

statementParser :: Parser Statement
statementParser = (A.skipSpace *>) $
  IfStatement <$> (A.string "if" *> A.skipSpace *> A.char '(' *> exprParser <* A.skipSpace <* A.char ')') <*> (A.skipSpace *> A.char '{' *> many statementParser <* A.skipSpace <* A.char '}' <|> pure <$> statementParser) <*> optional (A.skipSpace *> A.char '{' *> many statementParser <* A.skipSpace <* A.char '}' <|> pure <$> statementParser) <|>
  FCall <$> nameParser <*> paramParser <* A.skipSpace <* A.char ';' <|>
  Return <$> (A.string "return" *> A.skipSpace *> A.char '(' *> nameParser) <*> paramParser <* A.skipSpace <* A.char ')' <* A.skipSpace <* A.char ';' <|>
  Set <$> (A.string "set" *> nameParser) <*> (A.skipSpace *> A.char '=' *> A.skipSpace *> exprParser)

-- eqOpParser :: Parser EqOp
-- eqOpParser = (A.skipSpace *>) $
--   EqualOp <$ A.string "==" <|>
--   NotEqualOp <$ A.string "!=" <|>
--   GreaterEqualOp <$ A.string ">=" <|>
--   LessEqualOp <$ A.string "<=" <|>
--   GreaterOp <$ A.char '>' <|>
--   LessOp <$ A.char '<'

stringParser :: Parser Text
stringParser = A.char '"' *> A.takeWhile (/= '"') <* A.char '"' -- TODO

-- binOpParser :: Parser BinOp
-- binOpParser = (A.skipSpace *>) $
--   Plus <$ A.char '+' <|>
--   And <$ A.string "&&" <|>
--   Or <$ A.string "||"

opTable :: OperatorTable Text Expression
opTable = [
    [Prefix (A.skipSpace *> (Not <$ A.char '!'))]
  , [Infix (A.skipSpace *> (Bin Plus <$ A.char '+')) AssocLeft]
  ]

termParser :: Parser Expression
termParser = (A.skipSpace *>) $
  VarLit <$> nameParser <|>
  StringLit <$> stringParser <|>
  NumLit <$> A.decimal  <|>
  --Not <$> (A.char '!' *> exprParser) <|>
  -- (\a op b -> Compare op a b) <$> exprParser <*> eqOpParser <*> exprParser -- <|>
  --Match <$> nameParser <*> (A.skipSpace *> A.char '~' *> A.skipSpace *> stringParser) <|>
  --Assignment <$> nameParser <*> (A.skipSpace *> A.char '=' *> exprParser) <|>
  --(\a op b -> Bin op a b) <$> (StringLit <$> stringParser) <*> binOpParser <*> (VarLit <$> nameParser)
  A.char '(' *> A.skipSpace *> exprParser <* A.skipSpace <* A.char ')'

exprParser = buildExpressionParser opTable termParser

main :: IO ()
main = do
  print $ A.parse topParser [text|

sub policy_recv {
  if (!req.http.x-finn-apikey) {
    std.log("vcs-key:no_key");
    std.log("vcs-key:no_key__" + client.ip);
  }
}  

|]
