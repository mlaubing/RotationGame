module Main where

import Lib

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment

main :: IO()
main = do
	hSetBuffering stdout NoBuffering
	let b = [getRow (createMapIndices xend yend) n | n <- [0..xend]]
	let c = [createString l xend yend p t [] | l <- b]
	putStrLn(formatString(c))
	loop b c p t xend yend
    where p    = (2,2) :: (Int, Int)
          t    = (18,23) :: (Int, Int)
          xend = 20 :: Int
          yend = 27 :: Int