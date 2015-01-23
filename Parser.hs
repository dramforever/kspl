module Parser where

import Lexer
import qualified Data.Text.Lazy as T

data Piece = ErrorPiece String
           | CommandPiece T.Text
           | TextPiece T.Text
           | GroupPiece [Piece]

instance Show Piece where
  show (ErrorPiece err) = "<Error:" ++ err ++ ">"
  show (CommandPiece cmd) = "<\\" ++ T.unpack cmd ++ ">"
  show (TextPiece txt) = "<" ++ T.unpack txt ++ ">"
  show (GroupPiece grp) = "{" ++ show grp ++ "}"

(>+-) :: Piece -> ([Piece], [Token]) -> ([Piece], [Token])
p >+- ~(ps, ts) = (p : ps, ts)

parseTokens :: [Token] -> ([Piece], [Token])
parseTokens [] = ([], [])
parseTokens (PlainText t : ts) = TextPiece t >+- parseTokens ts
parseTokens (Command c : ts) = CommandPiece c >+- parseTokens ts
parseTokens ts@(ClosingBrace : _) = ([], ts)
parseTokens toks@(OpeningBrace : ts) =
  let (ps, ts1) = parseTokens ts
  in case ts1 of
   ClosingBrace : ts2 -> GroupPiece ps >+- parseTokens ts2
   [] -> ([ErrorPiece $
           "No matching closing brace for near " ++ show (take 5 toks)
          , GroupPiece ps], [])
         
   _ -> error "This can't happen! See Parser.hs"

