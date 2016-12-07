module Types where

import Data.List (intercalate, elemIndices, transpose, reverse)

-- MapObject ------------------------------------------------------
data MapObject = MapObject Char String Int Int deriving( Eq )

kind :: MapObject -> Char
kind (MapObject k _ _ _) = k

description :: MapObject -> String
description (MapObject _ descript _ _) = descript

yobj :: MapObject -> Int
yobj (MapObject _ _ y _) = y

xobj :: MapObject -> Int
xobj (MapObject _ _ _ x) = x

instance Show MapObject where
    show map = 
        (description map) ++ ": " ++ (show (kind map)) ++ 
                          "\nx: " ++ (show (xobj map)) ++ 
                          "\ny: " ++ (show (yobj map)) 
-------------------------------------------------------------------




-- Map ------------------------------------------------------------
data Map = Map {n :: Int, u :: Int, objs :: [[MapObject]]}
instance Show Map where
    show (Map {n=yend, u=xend, objs=objs}) = intercalate "" [(s++"\n") | s <- [[kind o | o <- obj] | obj <- objs]]

-- Erhalte die größere Dim der Map
getbiggerdimension map | (n map) > (u map) = n map
                       | otherwise         = u map
-- Transponiere die Mapelemente
transposemap map = map {objs=transpose (objs map)}

-- Drehen die Reihenfolge der Mapelemente um
reversemap map = map {objs=[reverse row | row <- objs map]}

-- Erzeuge eine Zeile mit Walls bei 1. und letzter Spalte und mittendrin nur bei 1. und letzter Spalte
createrowofoneobject f xs y | (head xs == 0) = map f xs
                            | otherwise      = ((MapObject 'W' "Wall" y 0) :
                                              (map f xs)) ++
                                              [(MapObject 'W' "Wall" y (last xs + 1))]

-- Benutzt "createrowofoneobject" und kombiniert sie rekursiv zu einer [[MapObject]] im Zeilenmuster der Map
mapfactory ni n u | ni==n     = [createrowofoneobject (MapObject 'W' "Wall" n) [0..u] n]
mapfactory ni n u | ni==0     = [createrowofoneobject (MapObject 'W' "Wall" ni) [0..u] ni] ++ mapfactory (ni+1) n u
mapfactory ni n u | otherwise = [createrowofoneobject (MapObject ' ' "Free" ni) [1..(u-1)] ni] ++ mapfactory (ni+1) n u

-- Erhalte alle freien Felder ZEILENWEISE im Format [[MapObject]]
getfreefieldsinrow mp = [(map (\elem -> getindices elem) (filter (\elem -> kind elem == ' ') row)) | row <- (objs mp)]

-- Erhalte alle freien Felder im Format [MapObject]
getallfreefields mp = (intercalate [] (getfreefieldsinrow mp))

-- Erhalte anhand einer [(Int, Int)], ob diese Felder frei sind als Bool
checkiffieldisfree [] mp = []
checkiffieldisfree (x:xs) mp = (x `elem` (getallfreefields mp)) : checkiffieldisfree xs mp

-- Benutzt "checkiffieldisfree" und wandelt die freien Felder in eine Tupleindizeliste um
getfreeindicesbytuplelst lst mp = map fst (filter (\x -> snd x == True) (zip lst (checkiffieldisfree lst mp)))

-- Erhalte (y,x) eines MapObject
getindices obj = (yobj obj, xobj obj)

-- Erhalte (y,x) des Spielers innerhalb der Map
findplayer mp = getindices $ head $ filter (\e -> kind e == 'O') (intercalate [] (objs mp))

-- Füge Elemente in erzeugten [[MapObject]] der Map ein                                   
insert y x k d objs = [[if (yobj o) /= y || (xobj o) /= x then o else (MapObject k d y x) | o <- obj] | obj <- objs]

-- Benutzt "insert" und fügt Spieler bei (1,1) und Ziel (rows-1, cols-1) ein
initmapobjectlist player rows cols = ((insert (rows-1) (cols-1) 'X' "Target") . (insert (fst player) (snd player) 'O' "Player")) (mapfactory 0 rows cols)

-- Benutzt "initmapobjectlist" um die veränderte Map zurück zu geben
changemap t map = (map {objs=initmapobjectlist t (n map) (u map)})

-- Benutzt "insert" und fügt eine neue Wall ein
insertwall x mp = mp {objs=insert (fst x) (snd x) 'W' "Wall" (objs mp)}

-- Benutzt "insertwall" und erzeugt anhand einer Tupleindizeliste alle Walls in der Map
insertwalls :: [(Int, Int)] -> Map -> Map
insertwalls [] mp = mp
insertwalls (x:xs) mp = ((insertwalls xs) . (insertwall x)) mp

-- Benutzt "transposemap" und "reversemap" um die Map zu drehen anhand einer Eingabe
rotate map 'v' = map
rotate map 'r' = (transposemap . reversemap) map
rotate map 'l' = (reversemap . transposemap) map
rotate map 'h' = (transposemap . reversemap . transposemap . reversemap) map
rotate map _ = map
-------------------------------------------------------------------




-- PositionList ---------------------------------------------------
data PositionList a = End | Step a (PositionList a) deriving (Show, Eq)

-- Fügt eine neue Position hinzu
insertposition u elem = u `Step` elem

-- Erhalte alle Step Tuples, die gegangen wurden
getsteps :: PositionList a -> [a]
getsteps End = []
getsteps (Step u next) = [u] ++ getsteps next

-- Erhalte die absolute Position des Spielers in der Map
getposition xs = (sum (map (fst) xs), sum (map (snd) xs))

-- Erhalte den Step-Tuple zu einer bestimmten Eingabe
getsinglestepcoords c | c =='v' = (-1, 0)
                      | c =='h' = ( 1, 0)
                      | c =='l' = ( 0,-1)
                      | c =='r' = ( 0, 1)
-------------------------------------------------------------------