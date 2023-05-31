{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use <$>" #-}

module Parser (parse_expr, parse_code) where

import Expr

import Control.Monad ()
import Control.Applicative ( Alternative(..) )
import Data.Char ( isAlpha )

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
            Just (x, xs) -> parse (f x) xs)

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
parse_expr input = maybe (error "Fail") fst $ parse applicationParser input

--- parse an atom => <variable> | '\' <variable> '.' <expr> | '(' <expr> ')' | '$' <expr> ---

atomParser :: Parser Expr
atomParser = variableParser <|> functionParser <|> parenthesizedParser <|> macroParser

--- parse a predicate => <predicate> ---

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \case
    "" -> Nothing
    (x : xs) -> if p x then Just (x, xs) else Nothing

--- parse an identifier => <identifier> ---

identifierParser :: Parser String
identifierParser = some $ predicateParser isAlpha

--- parse a character => <char> ---

charParser :: Char -> Parser Char
charParser c = predicateParser (== c)

--- parse a variable => <identifier> ---

variableParser :: Parser Expr
variableParser = Variable <$> identifierParser

--- parse a function => '\\' <identifier> '.' <expr> ---

functionParser :: Parser Expr
functionParser = Function <$> (charParser '\\' *> identifierParser <* charParser '.')
                          <*> atomParser

--- parse a parenthesized expression => '(' <expr> ')' ---

parenthesizedParser :: Parser Expr
parenthesizedParser = charParser '(' *> applicationParser <* charParser ')'

--- parse an application => <expr> <expr> ---

applicationParser :: Parser Expr
applicationParser = foldl Application <$> atomParser
                                      <*> many (charParser ' ' *> atomParser)

--- parse a macro => '$' <identifier> ---

macroParser :: Parser Expr
macroParser = Macro <$> (charParser '$' *> identifierParser)

--- 4.2. parse code ---

parse_code :: String -> Code
parse_code input = maybe (error "Fail") fst $ parse (assignParser <|> evaluateParser) input

--- parse an evaluation => <expr> ---

evaluateParser :: Parser Code
evaluateParser = Evaluate <$> applicationParser

--- parse an assignment => <identifier> '=' <expr> ---

assignParser :: Parser Code
assignParser = Assign
                <$> (identifierParser <* many (charParser ' ') <* charParser '=')
                <*> (many (charParser ' ') *> applicationParser)
