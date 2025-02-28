--Tuplas
tupla1:: (String, Int)
tupla1 =
  ("Tuple Size: ", 2)


--Método para listas
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even


main :: IO()
main = do
    
    let (entrada1, tamaño) = tupla1
    putStrLn("Tuple: " ++ show tupla1)
    putStrLn(entrada1 ++ show tamaño) --Show se usa para convertir de un Int a String
    
    let numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 
    let pares = filtrarPares numeros
    putStrLn("List: " ++ show numeros)
    putStrLn ("Even numbers in the list: " ++ show pares)


