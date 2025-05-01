module Lib (
    exitWithError,
    parseUInt,
    parseInt,
    parseTuple,
    parseSome,
    parseChar,
    parseAnyChar,
    parseMany,
    parseAnd,
    parseAndWith,
    parseOr,
    parseString,
    Parser(..),
    (<|>)
) where

import Data.Maybe
import System.Exit
import System.IO (hPutStrLn, stderr)
import Control.Applicative (Alternative(..), (<|>))
import Control.Monad (Monad(..))

--sortie d'erreur
exitWithError :: String -> IO a
exitWithError msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

data Parser a = Parser {
    runParser :: String -> Maybe(a, String)
}

instance Functor Parser where
    fmap f (Parser parser) = Parser $ \input ->
        case parser input of
            Nothing -> Nothing
            Just (result, rest) -> Just (f result, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    pf <*> px = Parser $ \input ->
        case runParser pf input of
            Nothing -> Nothing
            Just (f, rest) -> runParser (f <$> px) rest

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Just result -> Just result
            Nothing -> runParser p2 input

instance Monad Parser where
    p >>= f = Parser $ \input ->
        case runParser p input of
            Nothing -> Nothing
            Just (result, rest) -> runParser (f result) rest

--type Parser a = String -> Maybe(a, String)
parseChar :: Char -> Parser Char
parseChar c = Parser (\input ->
    case input of
        [] -> Nothing
        (x:xs) ->
            if c == x
            then Just (c, xs)
            else Nothing)

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser (\input ->
    case input of
        [] -> Nothing
        (b:bs) ->
            case chars of
                [] -> Nothing
                (x:xs) ->
                    if x == b
                    then Just (x, bs)
                    else runParser (parseAnyChar xs) input)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser (\input ->
    case runParser p1 input of
        Just result -> Just result
        Nothing -> runParser p2 input)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser (\input ->
    case runParser p1 input of
        Just (done1, rest1) ->
            case runParser p2 rest1 of
                Just (done2, rest2) -> Just ((done1, done2), rest2)
                Nothing -> Nothing
        Nothing -> Nothing)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fx p1 p2 = Parser (\input ->
    case runParser (parseAnd p1 p2) input of
        Just ((var, val), rest2) -> Just (fx var val, rest2)
        Nothing -> Nothing)

parseMany :: Parser a -> Parser [a]
parseMany p1 = Parser (\input ->
    case runParser p1 input of
        Nothing -> Just ([], input)
        Just (result, rest) ->
            case runParser (parseMany p1) rest of
                Just (results, lastrest) -> Just (result : results, lastrest))

parseSome :: Parser a -> Parser [a]
parseSome p1 = Parser (\input ->
    case runParser p1 input of
        Nothing -> Nothing
        Just (res, rest) ->
            case runParser (parseMany p1) rest of
                Just (res1, trest) -> Just (res : res1, trest))

parseUInt :: Parser Int
parseUInt = Parser (\input ->
    case runParser (parseSome (parseAnyChar ['0'..'9'])) input of
        Just (nb, oth) -> Just (read nb, oth)
        Nothing -> Nothing)

parseInt :: Parser Int
parseInt = Parser (\input ->
    case runParser (parseChar '-') input of
        Nothing -> runParser parseUInt input
        Just ('-', rest) ->
            case runParser parseUInt rest of
                Nothing -> Nothing
                Just (n, res) -> Just (-n, res))

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = Parser (\input ->
    case runParser (parseChar '(') input of
        Nothing -> Nothing
        Just (_, rest1) ->
            case runParser p rest1 of
                Nothing -> Nothing
                Just (a1, rest2) ->
                    case runParser (parseChar ',') rest2 of
                        Nothing -> Nothing
                        Just (_, rest3) ->
                            case runParser p rest3 of
                                Nothing -> Nothing
                                Just (a2, rest4) ->
                                    case runParser (parseChar ')') rest4 of
                                        Nothing -> Nothing
                                        Just (_, rest5) ->
                                            Just ((a1, a2), rest5))

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs