module Main where

import Lib

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment

main :: IO()
main = do
	hSetBuffering stdout NoBuffering
	let p = (2,2) :: (Int, Int)
	let t = (18,23) :: (Int, Int)
	let xstart = 0 :: Int
	let xend = 20 :: Int
	let ystart = 0 :: Int
	let yend = 27 :: Int
	let b = [getRow (createMapIndices xstart xend ystart yend) n | n <- [xstart..xend]]
	let c = [createString l xstart xend ystart yend p t [] | l <- b]
	
	putStrLn(formatString(c))
	loop b c p t xstart xend ystart yend

