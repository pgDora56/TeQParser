{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- complete to make calColumnSize and other functions
-- next -> tex file to calcolumnsize 
main :: IO ()
main = do 
    handle <- openFile "test.csv" ReadMode
    text <- T.hGetContents handle
    hClose handle
    handlef <- openFile "format.tex" ReadMode
    fmt <- T.hGetContents handlef
    hClose handlef
    putStrLn $ intercalate (T.unpack ",") (map show (calColumnSize text))
    T.writeFile "output/test.tex" (makeTex text fmt)
    putStrLn "Parse successfly."

-- For debug
--

makeTex :: T.Text -> T.Text -> T.Text
makeTex t f = T.replace "{table}" (makeTable t) f

makeTable :: T.Text -> T.Text
makeTable t = T.unlines ["\\begin{table}[h]", makeTabular t, (makeContent t), "\\end{tabular}", "\\end{table}"]

makeTabular :: T.Text -> T.Text
makeTabular t = foldr T.append "}" ["\\begin{tabular}{", "cccc"]
            where columns = calColumnSize t

makeContent :: T.Text -> T.Text
makeContent t = T.unlines l
                where l = map (\x -> T.append x " \\\\") $ T.lines (repAmp t)

repAmp :: T.Text -> T.Text
repAmp t = T.replace "," " & " t

calColumnSize :: T.Text -> [Int]
calColumnSize t = foldr colMaxiSize (replicate cnts 0) tlist
                where 
                    cnts = foldr max 0 (map countComma tlist)
                    tlist = T.lines t

colMaxiSize :: T.Text -> [Int] -> [Int]
colMaxiSize x xs = zipWith max xs (map T.length (T.splitOn "," x))

countComma :: T.Text -> Int
countComma t = T.count "," t
