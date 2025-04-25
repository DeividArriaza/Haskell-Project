import Data.Maybe (isNothing, listToMaybe)
import Data.List (transpose)

type Celda = Maybe Int
type Tablero = [[Celda]]
type Pos = (Int, Int)

encontrarVacia :: Tablero -> Maybe Pos
encontrarVacia t = listToMaybe [ (f, c) | f <- [0..8], c <- [0..8], isNothing (t !! f !! c) ]

reemplazar :: Tablero -> Pos -> Celda -> Tablero
reemplazar t (fila, col) val =
  take fila t ++
  [take col (t !! fila) ++ [val] ++ drop (col + 1) (t !! fila)] ++
  drop (fila + 1) t

esValido :: Tablero -> Pos -> Int -> Bool
esValido t (f, c) n =
  notElem (Just n) (t !! f) &&
  notElem (Just n) (map (!! c) t) &&
  notElem (Just n) (cuadro3x3 (f, c) t)

cuadro3x3 :: Pos -> Tablero -> [Celda]
cuadro3x3 (f, c) t =
  [ t !! r !! s | r <- [fBase..fBase+2], s <- [cBase..cBase+2] ]
  where
    fBase = f - f `mod` 3
    cBase = c - c `mod` 3

resolver :: Tablero -> [Tablero]
resolver t = case encontrarVacia t of
  Nothing -> [t]
  Just pos ->
    [ solucion
    | n <- [1..9], esValido t pos n,
      solucion <- resolver (reemplazar t pos (Just n)) ]

mostrarTablero :: Tablero -> IO ()
mostrarTablero = mapM_ putStrLn . map (unwords . map mostrarCelda)
  where
    mostrarCelda Nothing = "."
    mostrarCelda (Just n) = show n

leerSudoku :: FilePath -> IO Tablero
leerSudoku ruta = do
  contenido <- readFile ruta
  let filas = lines contenido
      convertirLinea = map (\x -> if x == 0 then Nothing else Just x) . map read . words
  return (map convertirLinea filas)

main :: IO ()
main = do
  tablero <- leerSudoku "sudoku.txt"
  putStrLn "\nTablero inicial:"
  mostrarTablero tablero
  case resolver tablero of
    [] -> putStrLn "\nNo se encontró solución."
    (s:_) -> do
      putStrLn "\nSolución encontrada:"
      mostrarTablero s
