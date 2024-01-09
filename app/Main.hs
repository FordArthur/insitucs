module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)

import Data.List ( find, isPrefixOf ) 

import Engine (interpret)

repl :: IO ()
repl = do
    i <- getLine
    putStrLn . fst . interpret $ i
    repl

main :: IO ()
main = do
    i <- getArgs
    case find ("--" `isPrefixOf`) i of
        Just flag -> case flag of
            "--repl" -> repl
            "--execute" -> mapM_ (handle . interpret) . filter (not . ("--" `isPrefixOf`)) $ i
        Nothing -> readFile (head i) >>= (handle . interpret)
    where handle (out, code) = putStrLn out >> exitWith code
