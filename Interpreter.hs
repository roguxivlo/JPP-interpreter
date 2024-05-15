module Main where

import MyLatte.Abs
import MyLatte.ErrM
import MyLatte.Par
import System.Environment (getArgs)
import Types (typeCheck)
import Prelude (FilePath, IO, print, putStr, putStrLn, readFile)

runFile :: FilePath -> IO ()
runFile f = do
  s <- readFile f
  let tokens = myLexer s
  case pProgram tokens of
    Bad err -> do
      putStrLn err
    Ok (Program location stmts) -> do
      -- print (Program location stmts)
      result <- typeCheck (Program location stmts)
      print result

main :: IO ()
main = do
  file_path <- getArgs
  case file_path of
    [file] -> runFile file
    _ -> putStrLn "Usage: ./interpreter file.mlt"