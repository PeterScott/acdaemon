{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad

import Trie
import RestServer (runServer)

port :: Int
port = 8080

main :: IO ()
main = do
  -- Load the dictionary into memory
  wordsBS <- L.readFile "/usr/share/dict/words"
  forM_ (L.lines wordsBS) $ \word ->
      insertPair (B.concat (L.toChunks word)) 0

  -- Run HTTP server. URLs have the form
  -- /complete/[prefix][?callback=...]
  putStrLn $ "Serving on port " ++ (show port)
  runServer port
