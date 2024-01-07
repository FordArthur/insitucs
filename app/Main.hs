module Main where

import System.Environment (getArgs)
import System.Exit

import Data.List 

import Engine (interpret)

repl :: IO ()
repl = do
    i <- getLine
    putStrLn . fst . interpret $ i
    main

main :: IO ()
main = do
    i <- getArgs
    case find ("--" `isPrefixOf`) i of
        Just flag -> case flag of
            "--repl" -> repl
            "--execute" -> mapM_ (handle . interpret) . filter (not . ("--" `isPrefixOf`)) $ i
        Nothing -> readFile (head i) >>= (handle . interpret)
    where handle (out, code) = putStrLn out >> exitWith code
