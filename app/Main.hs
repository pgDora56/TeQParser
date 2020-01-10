{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do 
    handle <- openFile "test.csv" ReadMode
    text <- T.hGetContents handle
    hClose handle
    handlef <- openFile "format.tex" ReadMode
    fmt <- T.hGetContents handlef
    hClose handlef
    T.putStrLn (T.showList $ calColumnSize text)
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

-- Error...How to separate Csv to Array????? Not splitOn????
calColumnSize :: T.Text -> [Int]
calColumnSize t = foldr (\x zipWith max (map length (splitOn "," x))) (replicate cnts 0) tlist
                where 
                    cnts = map countComma tlist
                    tlist = T.lines t

countComma :: T.Text -> Int
countComma t = T.count "," t
