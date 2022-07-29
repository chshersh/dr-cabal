module Main (main) where

import Depper (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
