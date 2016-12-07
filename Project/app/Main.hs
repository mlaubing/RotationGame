module Main where

import Types
import IO

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Game started!"
    mainmenu mymap (Step (0,0) End)
    where rows    = 8
          cols    = 10
          player = (1,1)
          mymap   = (Map {n   = rows, 
                          u   = cols, 
                          objs= initmapobjectlist player rows cols})