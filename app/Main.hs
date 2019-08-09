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
  Not Expression |
  Assignment Variable Expression |
  StringLit Text |
  NumLit Integer
  deriving Show

data TopStatement = SubRoutine Text [Statement] deriving Show

data BinOp = Add | And | Or | Match | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual deriving Show

skipSpace :: Parser ()
skipSpace = A.skipSpace *> A.char '#' *> A.manyTill A.anyChar A.endOfLine *> skipSpace <|>
  A.skipSpace

nameParser :: Parser Text
nameParser = (skipSpace *>) . fmap T.pack $ (:) <$> (A.letter <|> A.char '_') <*> many (A.letter <|> A.char '_' <|> A.digit <|> A.char '.' <|> A.char '-')

topParser :: Parser VCL
topParser = fmap VCL . some $ SubRoutine <$> (skipSpace *> A.string "sub" *> nameParser) <*> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}')

paramParser :: Parser [Expression]
paramParser = skipSpace *> A.char '(' *> A.sepBy exprParser (skipSpace *> A.char ',' <* skipSpace) <* skipSpace <* A.char ')'

statementParser :: Parser Statement
statementParser = (skipSpace *>) $
  IfStatement <$> (A.string "if" *> skipSpace *> A.char '(' *> exprParser <* skipSpace <* A.char ')') <*> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}' <|> pure <$> statementParser) <*> optional (skipSpace *> A.string "else" *> (skipSpace *> A.char '{' *> many statementParser <* skipSpace <* A.char '}' <|> pure <$> statementParser)) <|>
  FCall <$> nameParser <*> paramParser <* skipSpace <* A.char ';' <|>
  Return <$> (A.string "return" *> skipSpace *> A.char '(' *> nameParser) <*> paramParser <* skipSpace <* A.char ')' <* skipSpace <* A.char ';' <|>
  Set <$> (A.string "set" *> nameParser) <*> (skipSpace *> A.char '=' *> skipSpace *> exprParser) <* skipSpace <* A.char ';'

-- eqOpParser :: Parser EqOp
-- eqOpParser = (skipSpace *>) $
--   EqualOp <$ A.string "==" <|>
--   NotEqualOp <$ A.string "!=" <|>
--   GreaterEqualOp <$ A.string ">=" <|>
--   LessEqualOp <$ A.string "<=" <|>
--   GreaterOp <$ A.char '>' <|>
--   LessOp <$ A.char '<'

stringParser :: Parser Text
stringParser = A.char '"' *> A.takeWhile (/= '"') <* A.char '"' -- TODO

opTable :: OperatorTable Text Expression
opTable = [
    [Prefix (skipSpace *> (Not <$ A.char '!'))]
  , [Infix (skipSpace *> (Bin Add <$ A.char '+')) AssocLeft]
  , [Infix (skipSpace *> (Bin Match <$ A.char '~')) AssocLeft]
  , [Infix (skipSpace *> (Bin And <$ A.string "&&")) AssocLeft]
  , [Infix (skipSpace *> (Bin Or <$ A.string "||")) AssocLeft]
  , [Infix (skipSpace *> (Bin Equal <$ A.string "==")) AssocLeft]
  , [Infix (skipSpace *> (Bin NotEqual <$ A.string "!=")) AssocLeft]
  , [Infix (skipSpace *> (Bin Less <$ A.char '<')) AssocLeft]
  , [Infix (skipSpace *> (Bin Greater <$ A.char '>')) AssocLeft]
  , [Infix (skipSpace *> (Bin LessEqual <$ A.string "<=")) AssocLeft]
  , [Infix (skipSpace *> (Bin GreaterEqual <$ A.string ">=")) AssocLeft]
  ]

termParser :: Parser Expression
termParser = (skipSpace *>) $
  VarLit <$> nameParser <|>
  StringLit <$> stringParser <|>
  NumLit <$> A.decimal  <|>
  --Not <$> (A.char '!' *> exprParser) <|>
  -- (\a op b -> Compare op a b) <$> exprParser <*> eqOpParser <*> exprParser -- <|>
  --Match <$> nameParser <*> (skipSpace *> A.char '~' *> skipSpace *> stringParser) <|>
  --Assignment <$> nameParser <*> (skipSpace *> A.char '=' *> exprParser) <|>
  --(\a op b -> Bin op a b) <$> (StringLit <$> stringParser) <*> binOpParser <*> (VarLit <$> nameParser)
  A.char '(' *> skipSpace *> exprParser <* skipSpace <* A.char ')'

exprParser :: Parser Expression
exprParser = buildExpressionParser opTable termParser

main :: IO ()
main = do
  print $ A.parseOnly topParser [text|

sub policy_recv {
  if (!req.http.x-finn-apikey) {
    std.log("vcs-key:no_key");
    std.log("vcs-key:no_key__" + client.ip);
  }
  # client: abb-as
  else if (req.http.x-finn-apikey == "VR4AOW6FjaDLasHz") {
    std.log("vcs-key: client__abb-as");
    set req.http.x-client = "abb-as";
    if(((req.url ~ "^/iad/search/job-.*"
      || req.url ~ "^/iad/ad/model/job-.*"
      || req.url ~ "^/iad/ad/job-.*"
      || req.url ~ "^/iad[/]?$"
      || req.url ~ "^/iad/image/size$"
      || req.url ~ "^/iad/ad/\d+/?$") && req.method ~ "^(GET|HEAD)$")) {
      # NOOP
    } else {
      std.log("vcs-key: client__abb-as_urldenied");
      std.log("TRACE: Valid client abb-as does not have access to this URL and/or method");
      return(synth(403, "Forbidden"));
    }
  } # done with abb-as
}

sub policy_deliver {
  if (!req.http.x-client) {
    std.log("DENIED: Unknown client.");
    std.log("Unknown key used was " + req.http.x-finn-apikey);
    set req.http.x-forbid-please = "true";
    return(restart);
  }

  else if (req.http.x-client == "abb-as") {
    # Client has orgIds
    if ((req.method == "GET") && ((req.url ~ "^/iad/ad/[^(model|\d)].*") || (req.url ~ "^/iad/search/[^/]+[/]?$") || (req.url ~ "^/classified-product-commerce/organisations/\d+/classifieds.*") || (req.url ~ "^/statistics/.*") || (req.url ~ "^/distribution/.*"))) {
      if (!resp.http.RESOURCE-OWNERS && resp.status < 400) {
        std.log("TRACE: DENIED. Client has orgId, but no R-O in backend response");
        set req.http.x-forbid-please = "resp-has-no-RO";
        return(restart);
      }
      if (!resp.http.RESOURCE-OWNERS && resp.status >= 400) {
        return(synth(resp.status, resp.reason));
      }
      set req.http.x-tmp = ","+resp.http.RESOURCE-OWNERS+",";
      if (!(req.http.x-tmp ~ ",1444156934,")) {
        std.log("TRACE: DENIED. Client doesn't have access");
        set req.http.x-forbid-please = "notowner";
        return(restart);
      }
    }

    std.log("TRACE: ACCEPT. Client has access.");
  }
}


|]
