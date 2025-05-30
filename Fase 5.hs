-- Fase 5 - Investigación de lenguaje, David Lopez 24730

-- Este programa implementa la Criba de Eratóstenes en Haskell,
-- un algoritmo eficiente para generar números primos menores que un límite dado.
-- Se aprovechan las características del lenguaje funcional:
--  - Evaluación perezosa (lazy evaluation)
--  - Expresividad declarativa
--  - Listas infinitas
--  - Funciones puras y recursión

-- Función principal: genera la lista infinita de números primos
primos :: [Integer]
primos = criba [2..]
  where
    -- La función 'criba' toma una lista de enteros y filtra los múltiplos del primer elemento (el primo actual).
    criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]
    criba [] = []

-- Función para obtener los primeros n números primos
primerosPrimos :: Int -> [Integer]
primerosPrimos n = take n primos

-- Función para obtener todos los primos menores que un número dado
primosHasta :: Integer -> [Integer]
primosHasta n = takeWhile (< n) primos

-- Función principal para mostrar resultados
main :: IO ()
main = do
  putStrLn "Primeros 20 números primos:"
  print (primerosPrimos 20)

  putStrLn "\nNúmeros primos menores que 100:"
  print (primosHasta 100)

{- Ejecución esperada:

Primeros 20 números primos:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

Números primos menores que 100:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-}
