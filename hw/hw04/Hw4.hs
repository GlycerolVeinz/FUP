module Hw4 where

import Control.Applicative
import Data.Char
import Parser


type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var v) = v
    show (App e1 e2) = "(" ++ show e1 ++ show e2 ++ ")"
    show (Lambda v body) = "(\\" ++ v ++ "." ++ show body ++ ")"


-- Define the main parser function readPrg
readPrg :: String -> Maybe Expr
readPrg input = case parse program input of
    Just (expr, _) -> Just expr
    Nothing -> Nothing

-- Parser for a complete program
program :: Parser Expr
program = do
    defs <- many (definition <* sep)
    mainExpr <- expr
    _ <- many (sat isSpace)
    return $ resolveDefs defs mainExpr

-- Parser for a single definition
definition :: Parser (Symbol, Expr)
definition = do
    var <- varParser
    _ <- sep
    _ <- string ":="
    _ <- sep
    expr <- expr
    return (var, expr)

-- Parser for an expression
expr :: Parser Expr
expr = varExpr <|> lambdaExpr <|> appExpr

-- Parser for a variable
varExpr :: Parser Expr
varExpr = Var <$> varParser

-- Parser for a variable name
varParser :: Parser Symbol
varParser = some (sat isAlphaNum)

-- Parser for a lambda expression
lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- char '('
    _ <- char '\\'
    var <- varParser
    _ <- char '.'
    body <- expr
    _ <- char ')'
    return $ Lambda var body

-- Parser for an application expression
appExpr :: Parser Expr
appExpr = do
    _ <- char '('
    e1 <- expr
    _ <- sep
    e2 <- expr
    _ <- char ')'
    return $ App e1 e2

-- Resolve definitions in the main expression
resolveDefs :: [(Symbol, Expr)] -> Expr -> Expr
resolveDefs defs mainExpr = foldr (\(var, defExpr) acc -> substitute var defExpr acc) mainExpr defs

-- Substitute variable with expression in an expression
substitute :: Symbol -> Expr -> Expr -> Expr
substitute var defExpr (Var v) = if v == var then defExpr else Var v
substitute var defExpr (App e1 e2) = App (substitute var defExpr e1) (substitute var defExpr e2)
substitute var defExpr (Lambda v body) = Lambda v (substitute var defExpr body)