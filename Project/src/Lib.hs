module Lib
    ( createMapIndices,
	  createString,
	  getRow,
	  formatString,
	  loop,
	  randomPositions
    ) where
	
import Data.List (transpose, reverse, unfoldr)
import System.Random
	
createMapIndices :: Int -> Int -> Int -> Int -> [(Int, Int)]
createMapIndices xstart xend ystart yend = [(x,y) | x <- [xstart..xend], y <- [ystart..yend]]

createString :: (Show a, Num a) => [(Int, Int)] -> Int -> Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> String
createString ls xstart xend ystart yend player target walls = [ 
                                                          if (fst t == fst player && snd t == snd player)
														  then 'O' 
													      else if (fst t == xstart || fst t == xend || snd t == ystart || snd t == yend) 
														       || (fst t `mod` 2 == 0 && snd t `mod` 2 == 0) 
														       || (t `elem` walls)
													           then 'W'
														       else if (fst target == fst t && snd t == snd target)
															      then 'X'
																  else ' '
													   | t <- ls]

getRow :: [(Int, Int)] -> Int -> [(Int, Int)]
getRow ls n = [ l | l <- ls, fst l == n]

--transposeMap :: [a] -> [a]
transposeMap map = transpose map

--reverseMap :: [a] -> [a]
reverseMap map = [reverse row | row <- map]

formatString :: [String] -> String
formatString [] = ""
formatString (x:xs) = x ++ "\n" ++ formatString xs
	
--rotate :: [String] -> Char -> [String]
rotate map 'v' = map
rotate map 'r' = transposeMap(reverseMap(map))
rotate map 'l' = reverseMap(transposeMap(map))
rotate map 'h' = transposeMap(reverseMap(transposeMap(reverseMap(map))))

temporarilyNewPosition :: (Int, Int) -> Char -> (Int, Int)
temporarilyNewPosition (a,b) 'v' = (a-1, b)
temporarilyNewPosition (a,b) 'r' = (a, b+1)
temporarilyNewPosition (a,b) 'l' = (a, b-1)
temporarilyNewPosition (a,b) 'h' = (a+1, b)

{-changeCoords :: (Int, Int) -> Char -> (Int, Int)
changeCoords (a,b) 'v' = (a, b)
changeCoords (a,b) 'r' = (b, a)
changeCoords (a,b) 'l' = (b, a)
changeCoords (a,b) 'h' = (b, a)

changeDirection :: Char -> Char
changeDirection 'v' = 'v'
changeDirection 'h' = 'h'
changeDirection 'l' = 'r'
changeDirection 'r' = 'l'

checkDirection :: [Char] -> Char -> [Char]
checkDirection [] c = if c /= 'h' && c /= 'v' then [c] else []
checkDirection ls c = if c `elem` ls && c /= 'h' && c /= 'v'
					  then c : ls 
					  else if length ls == 0
						   then ls
						   else tail ls-}

checkField :: [String] -> (Int, Int) -> Char
checkField map p = map !! fst p !! snd p

randomPositions g1 g2 n m = zip [e `mod` n | e <- randomlist n g1] [e `mod` m | e <- randomlist m g2]

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

--findPlayer :: [String] -> (Int, Int)
--findPlayer map rows = [a | a <- (zip [0..rows] [if e == [] then 0 else e !! 0 | e <- [elemIndices 'O' l | l <- map]]), snd a /= 0] !! 0

--loop :: [(Int, Int)] -> [String] -> (Int, Int) -> IO()
loop indMap map p t xstart xend ystart yend = do
	putStr "Move 'r', 'l', 'h', 'v': " 
	inp <- readLn :: IO Char
	g1 <- newStdGen
	g2 <- newStdGen
	let walls = take ((xend+yend)*xend) (randomPositions g1 g2 xend yend)
	let nextPos = temporarilyNewPosition p inp
	let charOfNextField = (checkField map nextPos)
	let buildMapAsString = [createString l xstart xend ystart yend nextPos t walls | l <- indMap]
	let rotatedMap = rotate buildMapAsString inp
	
	if charOfNextField /= 'W'
	then putStrLn (formatString rotatedMap)
	else putStrLn (formatString (rotate map inp))
	
	if (charOfNextField /= 'W' && charOfNextField /= 'X')
	then loop indMap buildMapAsString nextPos t xstart xend ystart yend
	else if charOfNextField == 'X'
	     then putStrLn "Congrats, you reached the target!"
	     else loop indMap map p t xstart xend ystart yend