module Main (main) where

import LogParser (parseMessage)

main :: IO ()
main = print (parseMessage "E 44 1 a")