module Main where

import Lib

main :: IO ()
main = do
  _ <- deployBosh
  return ()
