module Main (main) where

import Anubisat (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
