module IO where

import Types
import System.Random
import Data.List (unfoldr)

temporarilynewposition :: (Int, Int) -> Char -> (Int, Int)
temporarilynewposition (a,b) c | c =='v' = (a-1, b)
temporarilynewposition (a,b) c | c =='r' = (a, b+1)
temporarilynewposition (a,b) c | c =='l' = (a, b-1)
temporarilynewposition (a,b) c | c =='h' = (a+1, b)
                               | otherwise = (0,0)

randomPositions g1 g2 n = zip [(e `mod` (n-1))+1 | e <- randomlist n g1] [(e `mod` (n-1))+1 | e <- randomlist n g2]

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

spawnwalls g1 g2 player map = (spawnaroundplayer player g2) ++ ((randomPositions g1 g2 . getbiggerdimension) map)

spawnaroundplayer player gen = [ls !! (e `mod` 8) | e <- (randomlist 3 gen)]
    where posx = fst player
          posy = snd player
          ls = [(posx+1, posy),
                (posx-1, posy),
                (posx+1, posy+1),
                (posx-1, posy-1),
                (posx-1, posy+1),
                (posx+1, posy-1),
                (posx,posy+1),
                (posx,posy-1)]

                
checkend mp answer save | tc `elem` (getallfreefields mp) = loop (rotate (changemap tc mp) answer) furtherstep answer
                        | tc == target                    = do putStrLn "REACHED THE TARGET! YOU WON!!!"
                                                               mainmenu mp (Step (1,1) End)
                        | otherwise                       = loop (rotate (changemap player mp) answer) save answer
    where player      = (findobj 'O' mp)
          target      = (findobj 'X' mp)
          tc          = (temporarilynewposition player answer)
          furtherstep = (insertposition (getsinglestepcoords answer) save)
          
          
loop mp save c | c=='v' || c=='r' || c=='h' || c=='l' = 
                   do 
                       putStrLn "Move 'r', 'l', 'h', 'v': "
                       g1 <- newStdGen
                       g2 <- newStdGen
                       let walls = spawnwalls g1 g2 player mp
                           newmap = insertwalls (getfreeindicesbytuplelst walls mp) mp
                       putStrLn (show newmap)
                       answer <- readLn :: IO Char
                       checkend newmap answer save
                | otherwise = mainmenu mp save
                where player = (findobj 'O' mp)
                 
              
mainmenu map save = do
    putStrLn "-------------"
    putStrLn "New Game  [1]"
    putStrLn "Load Game [2]"
    putStrLn "End       [_]"
    putStrLn "-------------\n"
    answer <- readLn :: IO Int
    if answer > 0 && answer < 3
    then if answer == 1 then loop (changemap (1,1) map) (Step (1,1) End) 'v'
         else do putStrLn "-------------------"
                 let playerpos = ((getposition . getsteps) save)
                 putStrLn ("Load last? " ++ (show playerpos) ++ "[1]?")
                 putStrLn "No              [2]"
                 putStrLn "-------------------"
                 answer <- readLn :: IO Int
                 if answer == 1 then loop (map {objs=initmapobjectlist playerpos (n map) (u map)}) save 'v'
                 else mainmenu map save
    else return ()