{-# LANGUAGE ViewPatterns #-}

module Lexer where

import qualified Data.Text.Lazy as T

import Data.Char
import Data.List

data Token = PlainText T.Text
           | Command T.Text
           | OpeningBrace
           | ClosingBrace

instance Show Token where
  show (PlainText t) = "<" ++ show t ++ ">"
  show (Command c) = "<\\" ++ T.unpack c ++ ">"
  show OpeningBrace = "<{>"
  show ClosingBrace = "<}>"

isSpecialChar :: Char -> Bool
isSpecialChar '{' = True
isSpecialChar '}' = True
isSpecialChar '\\' = True
isSpecialChar _ = False

lexer :: T.Text -> Maybe (Token, T.Text)
lexer (T.null -> True) = Nothing
lexer (T.uncons -> Just ('{', remaining)) = Just (OpeningBrace, remaining)
lexer (T.uncons -> Just ('}', remaining)) = Just (ClosingBrace, remaining)

lexer (T.uncons ->
       Just ('\\', T.uncons ->
             Just ('<', T.span isAlpha -> (h, re) ))) =
  
  case T.breakOn h re of
   (t, remaining) -> Just (PlainText t, remaining)

lexer (T.uncons -> Just ('\\',
                         T.uncons -> Just (h, remaining)
                        )) | not (isAlphaNum h) =
                               Just (Command (T.singleton h), remaining)

lexer (T.uncons -> Just ('\\',
                         T.span isAlphaNum -> (cmd, remaining)
                        )) = Just (Command cmd, remaining)

lexer (T.break isSpecialChar -> (text, remaining)) =
  Just (PlainText text, remaining)

lexAll :: T.Text -> [Token]
lexAll = unfoldr lexer
