module Main where

import qualified Data.Map as M (toList, fromListWith)
import qualified Data.List.Split as S
import qualified Data.Bifunctor as B
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.List as L
import System.Directory
import System.Random


-- El backtracking se implementa mediante el uso de la
-- implementación Alternative (<|>) del tipo Maybe
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

-- Declaración de los diferentes tipos
type Posibles = V.Vector Int
data Cell = Fixed Int | Posible Posibles  deriving (Show, Eq)
type Sudoku = A.Array (Int, Int) Int
type Grid = A.Array (Int, Int) Cell

-- Diferentes ejemplos de sudokus
ejsudoku = "9 2 0 8 0 0 0 4 0\n0 0 1 3 4 0 0 0 6 \n4 0 3 0 5 6 8 0 0 \n2 1 0 0 6 4 5 0 0 \n0 0 4 5 0 0 0 0 0 \n6 0 5 1 0 2 4 7 0 \n0 0 8 0 2 0 0 1 3 \n0 4 0 0 1 0 0 0 0 \n0 9 7 6 0 3 2 0 4"
ejsudoku2 = "0 5 0 0 6 0 0 0 1 \n0 0 4 8 0 0 0 7 0 \n8 0 0 0 0 0 0 5 2 \n2 0 0 0 5 7 0 3 0 \n0 0 0 0 0 0 0 0 0 \n0 3 0 6 9 0 0 0 5 \n7 9 0 0 0 0 0 0 8 \n0 1 0 0 0 6 5 0 0 \n5 0 0 0 3 0 0 6 0"
ejsudoku3 = "0 0 0 0 6 0 0 8 0 \n0 2 0 0 0 0 0 0 0 \n0 0 1 0 0 0 0 0 0 \n0 7 0 0 0 0 1 0 2 \n5 0 0 0 3 0 0 0 0 \n0 0 0 0 0 0 4 0 0 \n0 0 4 2 0 1 0 0 0 \n3 0 0 7 0 0 6 0 0 \n0 0 0 0 0 0 0 5 0"

-- cargaSudoku: A partir de un string formado por números genera el sudoku,
-- en caso de cumplir con los requisitos
cargaSudoku :: String -> Maybe Sudoku
cargaSudoku text
    | L.length values == 81 = Just $ A.listArray ((1,1), (9,9)) ts
    | otherwise = Nothing
    where 
        ts = L.map read values
        values = L.filter (`L.elem` ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])  (words text)

-- posibilidades: calcula las diferentes posibilidades que puede tener una celda.
posibilidades :: Int -> Int -> Sudoku -> Posibles 
posibilidades f c a 
  | (A.!) a (f,c) /= 0 = V.fromList [(A.!) a (f,c)]
  | otherwise = V.fromList (L.filter ((flip L.notElem) xs) [1..9])
    where xs = [(A.!) a (i,c) | i <- [1..9], (A.!) a (i,c) /= 0] L.++
                [(A.!) a (f,j) | j <- [1..9], (A.!) a (f,j) /= 0] L.++ 
                [(A.!) a (i,j) | i <- [3*fs+1..3*fs+3], j <- [3*cs+1..3*cs+3], (A.!) a (i,j)/=0]
          fs = div (f-1) 3
          cs = div (c-1) 3


-- cargaCell: transformamos los valores dentro del sudoku a tipo Cell
cargaCell :: Sudoku -> [Cell]
cargaCell sudoku = 
    getCell (A.assocs sudoku)
    where
        getCell [] = []
        getCell (((i,j), v):vs)
            | V.length ps == 0 = []
            | V.length ps == 1 = Fixed (V.head ps):getCell vs
            | otherwise = Posible ps:getCell vs
                where
                    ps = posibilidades i j sudoku

-- cargaGrid: a partir de la lista de celdas generamos un Grid 
-- en caso de que cumpla los requisitos.
cargaGrid :: [Cell] -> Maybe Grid
cargaGrid c = case c of
    cs 
        | L.length cs /= 81 -> Nothing
        | otherwise -> Just $ A.listArray ((1,1), (9,9)) cs

-- minchoice: a partir de un grid, elegir la celda con 
-- menor número de posibilidades a cubrir.
minchoice :: Grid -> ((Int, Int), Posibles)
minchoice g = minchoice' [((i,j),x) | ((i,j), Posible x) <- A.assocs g]

minchoice' :: [((Int, Int), Posibles)] -> ((Int, Int), Posibles)
minchoice' (((i,j),v):gs) = L.foldr accum ((i,j), v) (((i,j),v):gs)
    where 
        accum (p, xs) r@((i,j),v) =
            if V.length xs < V.length v 
            then (p, xs)
            else r


-- Funciones para que a partir de un grid, generar un sudoku por cada
-- posibilidad en una celda elegida de antemano.
siguienteGrid :: Int -> Int -> Int -> Grid -> Sudoku
siguienteGrid f c v g = 
  A.array (A.bounds g) [((i,j), func i j) | i <- [1..9], j <- [1..9]]
  where 
    func i j
      | i == f && j == c = v
      | otherwise = cellvalue ((A.!) g (i,j))

siguienteGrid' :: ((Int, Int), Posibles) -> Grid -> [Sudoku]
siguienteGrid' ((f,c), vs) g  
    | V.null vs = []
    | otherwise = siguienteGrid f c (V.head vs) g:siguienteGrid' ((f,c), V.tail vs) g
 
cellvalue :: Cell -> Int
cellvalue (Fixed l) = l
cellvalue _ = 0

-- Comprobación de si el grid resultante es el resuelto o si es inválido
isGridSolved :: Grid -> Bool
isGridSolved s = L.null [x | Posible x <- A.elems s]

isInvalidGrid :: Grid -> Bool
isInvalidGrid s = L.any (\y -> snd y > 9) (M.toList (M.fromListWith (+) [(x, 1) | Fixed x <- A.elems s]))

-- solve: recibe un sudoku y devuelve un Grid en caso de haber solución
solve :: Sudoku -> Maybe Grid
solve sudoku = cargaGrid (cargaCell sudoku) >>= solve'
    where
        solve' g
            | isInvalidGrid g = Nothing
            | isGridSolved g = Just g
            | otherwise =
                let xs = siguienteGrid' (minchoice g) g
                in L.foldr ((<|>) . solve) Nothing xs


--Funciones para la ilustración del sudoku original como del grid final
showGrid :: Grid -> [[Int]]
showGrid g = S.chunksOf 9 [x | Fixed x <- A.elems g]

showSudoku :: Sudoku -> [[Int]]
showSudoku s = S.chunksOf 9 [x | x <- A.elems s]

linea :: IO ()
linea = do
    putStrLn "-------------------------"

lineaCell :: [Int] -> IO ()
lineaCell cs = do
    let text = [show x ++ " " ++ show y ++ " " ++ show z | [x,y,z] <- S.chunksOf 3 cs]
    let text2 = concat [x ++ " | " | x <- text]
    putStrLn ("| " ++ text2)

gridCell :: [[Int]] -> IO ()
gridCell css = do
    linea
    sequence_ [lineaCell x | x <- take 3 css]
    linea
    sequence_ [lineaCell x | x <- take 3 (drop 3 css)]
    linea
    sequence_ [lineaCell x | x <- drop 6 css]
    linea


-- Aletorizar el proceso de elección de posibilidades

-- newrandom: genera un nuevo número a partir de una semilla y en un rango
-- limitado por el valor l. Siendo l la longitud de la lista de opciones.
newrandom :: StdGen -> Int -> (Int, StdGen)
newrandom r l = randomR (0,l-1) r

-- randomchoice: A partir de un grid y de una semilla (para newrandom),
-- devuelve la opción a elegir y la nueva semilla a usar para la próxima opción.
randomchoice :: Grid -> StdGen -> (((Int, Int), Posibles), StdGen)
randomchoice g r = B.first (options !!) n 
    -- (options!!(fst n), snd n)
        where
            lmin = minimum [length x | Posible x <- A.elems g]
            options = filter (\((i,j), v) -> V.length v == lmin) [((i,j),x) | ((i,j), Posible x) <- A.assocs g]
            n = newrandom r (length options)
        
-- solve'': funcion similar que recibirá una nueva semilla en cada iteración para generar
-- una nueva opción.   
solve'' :: Sudoku -> StdGen -> Maybe Grid
solve'' sudoku r = cargaGrid (cargaCell sudoku) >>= solve'
    where
        solve' g
            | isInvalidGrid g = Nothing 
            | isGridSolved g = Just g
            | otherwise =
                let 
                    xs = siguienteGrid' (fst (randomchoice g r)) g
                in 
                    L.foldr accum Nothing xs
                        where
                            accum sudoku Nothing = solve'' sudoku (snd (randomchoice g r))
                            accum sudoku (Just o) = Just o

-- Con aletoriedad
main2 :: IO ()
main2 = do
    putStrLn "Indica nombre de fichero"
    fileName <- getLine 
    exists <- doesFileExist fileName
    input <- if exists then readFile fileName else return ""
    r <- newStdGen
    if L.null input then
        putStrLn "Fichero no existente"
    else do 
        case cargaSudoku input of
            Nothing -> putStrLn "Formato del sudoku incorrecto"
            Just sudoku -> do
                    case solve'' sudoku r of
                        Nothing -> putStrLn "El sudoku no tiene solucion"
                        Just res -> do
                            putStrLn "\nSudoku a resolver"
                            gridCell (showSudoku sudoku)
                            putStrLn "\nSudoku resuelto"
                            gridCell (showGrid res)

-- Sin aletoriedad
main :: IO ()
main = do  
    putStrLn "Indica nombre de fichero"
    fileName <- getLine 
    exists <- doesFileExist fileName
    input <- if exists then readFile fileName else return ""
    if L.null input then
        putStrLn "Fichero no existente"
    else do 
        case cargaSudoku input of
            Nothing -> putStrLn "Formato del sudoku incorrecto"
            Just sudoku -> do
                case solve sudoku of
                    Nothing -> putStrLn "El sudoku no tiene solucion"
                    Just grid -> do 
                        putStrLn "\nSudoku a resolver"
                        gridCell (showSudoku sudoku)
                        putStrLn "\nSudoku resuelto"
                        gridCell (showGrid grid)
                        -- sequence_ [gridCell x | x <- [showGrid grid, showSudoku sudoku]]
