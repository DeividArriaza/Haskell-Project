import System.IO
import Data.List (delete)

type Tarea = String

main :: IO ()
main = menu []  

menu :: [Tarea] -> IO ()
menu tareas = do
    putStrLn "\n--- Gestión de Tareas ---"
    putStrLn "1. Agregar tarea"
    putStrLn "2. Completar tarea"
    putStrLn "3. Ver tareas"
    putStrLn "4. Salir"
    putStr "Elige una opción: "
    hFlush stdout 

    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingresa la nueva tarea: "
            hFlush stdout
            nuevaTarea <- getLine
            menu (tareas ++ [nuevaTarea])  
        "2" -> do
            if null tareas
                then putStrLn "No hay tareas para completar."
                else do
                    putStrLn "\nTareas actuales:"
                    mostrarTareas tareas
                    putStr "Elige el número de la tarea a completar: "
                    hFlush stdout
                    numStr <- getLine
                    let num = read numStr :: Int
                    if num > 0 && num <= length tareas
                        then menu (eliminarTarea num tareas)
                        else do
                            putStrLn "Número inválido. Intenta de nuevo."
                            menu tareas
        "3" -> do
            putStrLn "\nTareas actuales:"
            if null tareas 
                then putStrLn "No hay tareas registradas."
                else mostrarTareas tareas
            menu tareas
        "4" -> putStrLn "Saliendo del programa..."
        _   -> do
            putStrLn "Opción no válida, intenta de nuevo."
            menu tareas

mostrarTareas :: [Tarea] -> IO ()
mostrarTareas tareas = mapM_ (\(i, tarea) -> putStrLn (show i ++ ". " ++ tarea)) (zip [1..] tareas)

eliminarTarea :: Int -> [Tarea] -> [Tarea]
eliminarTarea n tareas = take (n - 1) tareas ++ drop n tareas
