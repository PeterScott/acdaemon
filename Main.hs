{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment (getArgs)
import Control.Monad

import Trie

main :: IO ()
main = do
  -- Load the dictionary into memory
  wordsBS <- L.readFile "/usr/share/dict/words"
  forM_ (L.lines wordsBS) $ \word ->
      insertPair (B.concat (L.toChunks word)) 0

  -- Do a query for the prefix specified on the command line.
  args <- getArgs
  let prefix = B.pack $ if length args >= 1 then args !! 0 else ""
  autocomplete prefix >>= print
