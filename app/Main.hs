module Main where

import System.Environment ( getArgs )
import System.Exit ( exitWith )

import Data.List ( find, isPrefixOf ) 
import Control.Monad ( (>=>) )

import Engine
import Parser
import Types

repl :: IO ()
repl = do
    i <- getLine
    interpret i >>= putStrLn . fst
    repl

main :: IO ()
main = do
    i <- getArgs
    case find ("--" `isPrefixOf`) i of
        Just flag -> case flag of
            "--repl"    -> repl
            "--execute" -> mapM_ (interpret >=> handle) . filter (not . ("--" `isPrefixOf`)) $ i
        Nothing -> readFile (head i) >>= interpret >>= handle
    where handle (out, code) = putStrLn out >> exitWith code