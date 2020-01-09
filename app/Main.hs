{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do 
    handle <- openFile "test.csv" ReadMode
    text <- T.hGetContents handle
    T.writeFile "test.tex" (repAmp text)
    hClose handle

repAmp :: T.Text -> T.Text
repAmp t = T.replace "," "&" t

