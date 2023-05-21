{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}

module Parser (parse_expr, parse_code) where

import Expr

import Control.Monad
import Control.Applicative
import Data.Char ( isAlphaNum, isSpace, isAlpha )
import GHC.Base (Alternative((<|>)))

-- Parser data type

newtype Parser a = Parser {
    parse :: String -> Maybe (a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser (\input -> Just (x, input))
    (>>=) (Parser p) f = Parser (\input ->
        case p input of
            Nothing -> Nothing
            Just (x, remaining) -> parse (f x) remaining)

instance Applicative Parser where
    pure = return
    pf <*> px = do
        f <- pf
        f <$> px

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Nothing -> p2 input
            result -> result

--- type declaration over ---

--- parse an expression ---

parse_expr :: String -> Expr
parse_expr input =
    case parse exprParser input of
        Just (expr, _) -> expr
        Nothing -> error "Failed to parse expression"

exprParser :: Parser Expr
exprParser = applicationParser <|> atomParser

atomParser :: Parser Expr
atomParser = variableParser <|> functionParser <|> parenthesizedExprParser <|> macroParser

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p =
    Parser (
        \case
            [] -> Nothing
            (x : xs) -> if p x then Just (x, xs) else Nothing)

identifierParser :: Parser String
identifierParser = do  
    x <- predicateParser isAlpha
    xs <- startParser (predicateParser isAlphaNum)
    return ((: xs) x)

plusParser :: Parser a -> Parser [a]
plusParser p = do
    x <- p
    xs <- startParser p
    return ((: xs) x)

startParser :: Parser a -> Parser [a]
startParser p = plusParser p <|> return []

charParser :: Char -> Parser Char
charParser c =
    Parser (
        \case
            [] -> Nothing
            (x : xs) -> if x == c then Just (c, xs) else Nothing)

--- parse a variable => <variable> ---

variableParser :: Parser Expr
variableParser = Variable <$> identifierParser

--- parse a function => '\' <variable> '.' <expr> ---

functionParser :: Parser Expr
functionParser = do
    charParser '\\'
    name <- identifierParser
    charParser '.'
    Function name <$> atomParser

--- parse a parenthesized expression => '(' <expr> ')' ---

parenthesizedExprParser :: Parser Expr
parenthesizedExprParser = do
    charParser '('
    x <- exprParser
    charParser ')'
    return x

--- parse an application <expr> <expr> ---

applicationParser :: Parser Expr
applicationParser = do
    f <- atomParser
    rest <- let restParser = do
                    startParser (charParser ' ')
                    arg <- atomParser
                    rest <- restParser <|> return []
                    return (arg : rest)
            in restParser <|> return []
    return (foldl Application f rest)

--- parse a macro ---
macroParser :: Parser Expr
macroParser = do
    charParser '$'
    Macro <$> identifierParser

-- 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
