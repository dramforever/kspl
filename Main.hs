module Main where

import qualified Data.Text.Lazy.IO as T

import Lexer
import Parser
import Interpreter

main :: IO ()
main = T.getContents >>= (execPieces . parseTokens . lexAll) >>= T.putStr
