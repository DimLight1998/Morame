module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

import AST

langDef :: LanguageDef ()
langDef = emptyDef
    { identStart = letter
    , identLetter = alphaNum
    , commentStart = "{-"
    , commentEnd = "-}"
    , commentLine = "--"
    , nestedComments = False
    , opStart = oneOf "-!&|+*/%=<>:~"
    , opLetter = oneOf "&|=>"
    , reservedNames =
        [ "true"
        , "false"
        , "if"
        , "then"
        , "else"
        , "let"
        , "in"
        , "letrec"
        , "case"
        , "of"
        , "end"
        , "data"
        , "Int"
        , "Char"
        , "Bool"
        ]
    , reservedOpNames =
        [ "-", "+", "*", "/", "%", "~", "|"
        , "!", "&&", "||"
        , "==", "!=", "<", ">", "<=", ">="
        , "=>", "->", "="
        ]
    }

lexer = T.makeTokenParser langDef

identifier = T.identifier lexer
whiteSpace = T.whiteSpace lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
natural = T.natural lexer
charLiteral = T.charLiteral lexer
parens = T.parens lexer
brackets = T.brackets lexer
colon = T.colon lexer
semi = T.semi lexer

-- type parser

typeAnnotation :: Parser Type
typeAnnotation = let
    singleType :: Parser Type
    singleType =
            parens typeAnnotation
        <|> try (reserved "Int" >> return TInt)
        <|> try (reserved "Char" >> return TChar)
        <|> try (reserved "Bool" >> return TBool)
        <|> TData <$> identifier
    opTable = [[Infix (reservedOp "->" >> return TArrow) AssocRight]]
    in buildExpressionParser opTable singleType

-- pattern parser

patt :: Parser Pattern
patt = choice $ map try
    [ parens patt
    , boolLitPattern
    , intLitPattern
    , charLitPattern
    , dataPattern
    , varPattern
    ]

boolLitPattern :: Parser Pattern
boolLitPattern =
        (reserved "true" >> return (PBoolLit True))
    <|> (reserved "false" >> return (PBoolLit False))

intLitPattern :: Parser Pattern
intLitPattern =
        (PIntLit . fromInteger <$> natural) 
    <|> (PIntLit . fromInteger . negate <$> (reservedOp "~" >> natural))

charLitPattern :: Parser Pattern
charLitPattern = PCharLit <$> charLiteral

varPattern :: Parser Pattern
varPattern = PVar <$> identifier

dataPattern :: Parser Pattern
dataPattern = PData <$> identifier <*> brackets (many patt)

-- expression parser

opTable :: OperatorTable Char () Expr
opTable =
    [ [ Infix (return EApply) AssocLeft
      ]
    , [ Infix (reservedOp "*" >> return EMul) AssocLeft
      , Infix (reservedOp "/" >> return EDiv) AssocLeft
      , Infix (reservedOp "%" >> return EMod) AssocLeft
      ]
    , [ Infix (reservedOp "+" >> return EAdd) AssocLeft
      , Infix (reservedOp "-" >> return ESub) AssocLeft
      ]
    , [ Infix (reservedOp "==" >> return EEq) AssocLeft
      , Infix (reservedOp "!=" >> return ENeq) AssocLeft
      , Infix (reservedOp "<" >> return ELt) AssocLeft
      , Infix (reservedOp ">" >> return EGt) AssocLeft
      , Infix (reservedOp "<=" >> return ELe) AssocLeft
      , Infix (reservedOp ">=" >> return EGe) AssocLeft
      ]
    , [ Prefix (reservedOp "!" >> return ENot)
      ]
    , [ Infix (reservedOp "&&" >> return EAnd) AssocLeft
      , Infix (reservedOp "||" >> return EOr) AssocLeft
      ]
    ]

expr :: Parser Expr
expr = buildExpressionParser opTable singleExpr

singleExpr :: Parser Expr
singleExpr = choice $ map try
    [ boolLit
    , intLit
    , charLit
    , ifExpr
    , lambdaExpr
    , letExpr
    , letRecExpr
    , varExpr
    , caseExpr
    , parens expr
    ]

boolLit :: Parser Expr
boolLit =
        (reserved "true" >> return (EBoolLit True))
    <|> (reserved "false" >> return (EBoolLit False))

intLit :: Parser Expr
intLit =
        (EIntLit . fromInteger <$> natural) 
    <|> (EIntLit . fromInteger . negate <$> (reservedOp "~" >> natural))

charLit :: Parser Expr
charLit = ECharLit <$> charLiteral

ifExpr :: Parser Expr
ifExpr = EIf
    <$> (reserved "if" >> expr)
    <*> (reserved "then" >> expr)
    <*> (reserved "else" >> expr)

lambdaExpr :: Parser Expr
lambdaExpr = ELambda
    <$> parens ((,) <$> identifier <*> (colon >> typeAnnotation))
    <*> (reservedOp "=>" >> expr)

letExpr :: Parser Expr
letExpr = ELet
    <$> ((,) <$> (reserved "let" >> identifier) <*> (reservedOp "=" >> expr))
    <*> (reserved "in" >> expr)

letRecExpr :: Parser Expr
letRecExpr = do
    reserved "letrec"
    funcName <- identifier
    arg <- parens $ (,) <$> identifier <*> (colon >> typeAnnotation)
    retType <- colon >> typeAnnotation
    reservedOp "="
    funcBody <- expr
    reserved "in"
    ELetRec funcName arg (funcBody, retType) <$> expr

varExpr :: Parser Expr
varExpr = EVar <$> identifier

caseExpr :: Parser Expr
caseExpr = let
    branch :: Parser (Pattern, Expr)
    branch = (,) <$> patt <*> (reservedOp "->" >> expr)
    in do
        reserved "case"
        e <- expr
        reserved "of"
        branches <- sepBy branch semi
        return $ ECase e branches

-- ADT parser

adt :: Parser ADT
adt = let
    branch :: Parser (String, [Type])
    branch = (,) <$> identifier <*> brackets (many typeAnnotation)
    in do
        reserved "data"
        adtName <- identifier
        reservedOp "="
        branches <- sepBy branch (reservedOp "|")
        return $ ADT adtName branches

-- main parser

morameParser :: String -> Maybe Program
morameParser s = case parse (whiteSpace >> morameParser') "" s of
    Left _ -> Nothing
    Right p -> Just p

morameParser' :: Parser Program
morameParser' = Program <$> many adt <*> expr
