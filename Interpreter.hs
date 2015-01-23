{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Interpreter where

import Control.Monad.State
import Control.Applicative
import Control.DeepSeq

import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.IO      as T
import qualified Data.Text.Lazy.Builder as TB
import           Data.Monoid
import qualified Data.Map               as M

import System.IO

import Parser
import Lexer

newtype InterpreterM a =
  InterpreterM { run :: StateT (M.Map T.Text
                                ([Piece] -> InterpreterM [Piece])
                               ) IO a }
  deriving (Functor, Applicative, Monad, MonadIO
           , MonadState (M.Map T.Text ([Piece] -> InterpreterM [Piece])))

runMacro :: T.Text -> [Piece] -> InterpreterM [Piece]
runMacro name ps = do
  res <- gets $ M.lookup name
  case res of
   Just macro -> macro ps
   Nothing -> return $ (ErrorPiece $
                        "No macro \\" ++ T.unpack name
                        ++ " (deleted)"
                       ) : ps

runPieces' :: [Piece] -> InterpreterM TB.Builder
runPieces' [] = return mempty
runPieces' (ErrorPiece err : ps) =
  liftIO (hPutStrLn stderr $ err) >> runPieces' ps
runPieces' (TextPiece t : ps) = (TB.fromLazyText t <>) <$> runPieces' ps
runPieces' (GroupPiece g : ps) = runPieces' (g ++ ps)
runPieces' (CommandPiece cm : ps) = runMacro cm ps >>= runPieces'

runPieces :: ([Piece], [Token]) -> InterpreterM T.Text
runPieces (ps, ts) = do
  res <- runPieces' ps
  case ts of
   [] -> return ()
   _:_ ->
     liftIO (hPutStrLn stderr
             ("Unconsumed tokens near " ++ show (take 5 ts)))
  return (TB.toLazyText res)

defaultMap :: M.Map T.Text ([Piece] -> InterpreterM [Piece])
defaultMap =
  M.fromList [ ("Backslash", return . (TextPiece "\\" :))
             , ("OpeningBrace", return . (TextPiece "{" :))
             , ("ClosingBrace", return . (TextPiece "}" :))
             , ("S", (\ps ->
                       case ps of
                        a : b : c : xs ->
                          return (a : c : GroupPiece [b, c] : xs)
                          
                        _ -> return $
                             (ErrorPiece $
                              "No enough arguments to \\S, got:\n"
                              ++ "        " ++ show ps
                              ++ "\n    (command was deleted)") : ps ))
             , ("K", (\ps ->
                       case ps of
                        a : _ : xs ->
                          return (a : xs)
                        _ -> return $
                             (ErrorPiece $
                              "No enough arguments to \\K, got:\n"
                              ++ "        " ++ show ps
                              ++ "\n    (command was deleted)") : ps ))
             , ("Define", (\ps ->
                 case ps of
                   CommandPiece cmd : GroupPiece body : xs ->
                    xs <$ modify (M.insert cmd $ return . (body ++))
                   _ ->
                     return $ 
                     (ErrorPiece $
                      "Bad arguments to \\Define\n"
                      ++ "    Correct syntax: \\Define\\commandName{body}, "
                      ++ "but got:\n"
                      ++ "        " ++ show (take 2 ps)
                      ++ "\n    (command was deleted)") : ps))
             , ("Include", (\ps ->
                 case ps of
                  GroupPiece [TextPiece fileName] : xs -> do
                    contents <- liftIO $ withFile (T.unpack fileName) ReadMode
                                (\h -> do
                                    c <- T.hGetContents h
                                    rnf c `seq` return c)
                          
                                                                       
                    
                    let (pieces, _) = parseTokens $ lexAll contents
                    liftIO $ T.hPutStrLn stderr ("(<enter " <> fileName <> ">")
                    return $ pieces
                      ++ (CommandPiece "LeaveIncludedFile"
                          : TextPiece fileName : xs)
                      
                  _ ->
                    return $
                     (ErrorPiece $
                      "Bad arguments to \\Include\n"
                      ++ "    Correct syntax: \\Include{fileName}, "
                      ++ "but got:\n"
                      ++ "        " ++ show (take 1 ps)
                      ++ "\n    (command was deleted)") : ps))
             , ("LeaveIncludedFile",
                (\(TextPiece p:ps) -> do
                    ps <$ (liftIO . T.putStrLn $ "<leave " <> p <> ">)")))
             ]

execPieces :: ([Piece], [Token]) -> IO T.Text
execPieces ts = evalStateT (run $ runPieces ts) defaultMap
