module Main where

import App

main :: IO ()
main = do
  s <- appMain
  mapM_ putStrLn . sResults $ s
  return ()
