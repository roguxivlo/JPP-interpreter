module Main where

import MyLatte.Abs
import MyLatte.ErrM
import MyLatte.Par
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Types (typeCheck)
import Prelude (Either (..), FilePath, IO, print, putStr, putStrLn, readFile, show)

runFile :: FilePath -> IO ()
runFile f = do
  s <- readFile f
  let tokens = myLexer s
  case pProgram tokens of
    Bad err -> do
      hPutStrLn stderr (show err)
    Ok (Program location stmts) -> do
      -- print (Program location stmts)
      result <- typeCheck (Program location stmts)
      -- if an error occured, print it to stderr
      case result of
        Left e -> hPutStrLn stderr (show e)
        Right _ -> print result

main :: IO ()
main = do
  file_path <- getArgs
  case file_path of
    [file] -> runFile file
    _ -> putStrLn "Usage: ./interpreter file.mlt"