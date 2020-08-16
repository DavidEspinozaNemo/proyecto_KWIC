-- menu del proyecto III
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).

import Prelude hiding (null, lookup, map, filter)
import Data.Char
import Data.List (sort,map, nub, concat, filter)
import System.IO
import System.Directory
import Data.Typeable

-- Codigo profe
toWords :: [Char] -> [[Char]] 
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

lowercase = map toLower
lowercases = map lowercase

-- Filtra listas que tienen como primer elemento una palabra no significativa
getSignificants titles ns = filter (\x -> not (lowercase(head x) `elem` ns)) titles
-- getSignificants2 titles ns =  [ x | x <- titles, (not (lowercase(head x)) `elem` ns)]

-- Generar rotaciones para arreglo de strings. Recibe arreglo de strings y retorna lista arreglos con rotaciones
sigRotations xs = [ drop i xs ++ take i xs | i <- [0 .. n]]
                  where n = (length xs) - 1

-- Agrega espacios a elementos de una lista y los concatena
putSpaces xss = tail (concat (map (' ':) xss))

-- Agregar separador al final de una oracion
sep xs = init xs ++ [last xs ++ " ><"]

-- Generar rotaciones a partir de un titulo y agregar espacios a cada elemento
kwic title notSignificants = do let titleRotations = sigRotations (sep (toWords title))
                                let filteredRotations = getSignificants titleRotations notSignificants
                                map putSpaces (filteredRotations)

-- Agrega un salto de linea a strings de una lista
addNewLineOutputs lista = map (\x -> concat(x : [" \n "])) lista

-- Imprime rotaciones para titulos
printRotations ts ns = map sigRotations (map sep (map toWords ts))

-- Cargar palabras de archivo de texto en lista.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)

-- Cargar lineas de archivo de texto en lista.
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)

-- For every element in a list of titles, generate rotations and add spaces and concatenate the words in each rotation
generateAllKwic ts ns = map (`kwic` ns) ts

-- For every rotation, add a new line
formatKwicOutput ts ns = concat(addNewLineOutputs (nub(sort(concat(generateAllKwic ts ns)))))

-- Escribir contenido en path
escribir :: FilePath -> String -> IO()
escribir path contenido = writeFile path (contenido)

-- resultado alineado 
-- 0. Volver todo el string en minusculas.
--    A partir de aqui ir por lineas, y rehacer el string
-- 1. Obtener la palabra importante (la primera)
-- 1.1 Volverla en mayuscula
-- 2. Avanzar hasta pegar con ><, vamos pegando todo a la derecha de la palabra principal
-- 3. Ir contando los letras restantes, y las vamos pegando a la izquierda de la principal
-- 3.1 guardamos todo en un arreglo
-- 3.2 obtenemos el mas largo, sin afectar el orden
-- 4. la funcion que aplica todo necesita; el string resultante de 3., 3.1, y 3.2
--     A partir de aqui va linea a linea
-- 4.1 toma una linea, el correspodiente de 3.1, y el 3.2
-- 4.2 agrega espacios en blanco, segun la diferencia entre 3.1 y 3.2
-- 4.3 rehace el string, linea por linea
-- fin del resultado alineado

type Estado = Map String Integer

main :: IO ()
main = do 
       mainloop (Map.fromList[])

menuENG :: [String] -> [String] -> String -> IO ()
menuENG titles notsignificants outputPath = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "                Welcome to the english menu                "
  putStrLn "-----------------------------------------------------------"
  putStrLn ">>> Please select one of the following options: "
  putStrLn "1- titles          (The entered path will be read)" 
  putStrLn "2- notSignificant  (The entered path will be read)" 
  putStrLn "3- setOutput       (The entered path will be used to store results)"
  putStrLn "4- basic           (Basic rotation will be applied and stored in selected output)"
  putStrLn "5- align           (Align rotation will be applied and stored in selected output)"
  putStrLn "6- exit"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "1" -> do
                putStrLn ">>> Enter the name of the title file: "
                putStr ">> "
                nombreArchivo <- getLine
                -- mapM_ print titles
                exists1 <- doesFileExist nombreArchivo
                if exists1 then do
                  titles <- getLines nombreArchivo
                  putStrLn $ "File " ++ nombreArchivo ++ " was loaded"
                  menuENG titles notsignificants outputPath
                else do
                  putStrLn $ "The file doesn´t exist"
                  menuENG [] notsignificants outputPath
     "2" -> do
                putStrLn ">>> Enter the name of the non-significant type file: "
                putStr ">> "
                nombreArchivo <- getLine
                -- mapM_ print nosignificativos
                exists2 <- doesFileExist nombreArchivo
                if exists2 then do
                  nosignificativos <- getWords nombreArchivo
                  putStrLn $ "File " ++ nombreArchivo ++ " was loaded" 
                  menuENG titles notsignificants outputPath
                else do
                  putStrLn $ "The file doesn´t exist"
                  menuENG titles [] outputPath
     "3" -> do
                putStrLn ">>> Output file name: "
                putStr ">> "
                outputPath <- getLine
                -- Revisar si el archivo existe
                existe <- doesFileExist outputPath
                -- Si el archivo existe, imprime mensaje, sino escribir resultado en path de salida. Parametros: path, contenido
                if existe then do 
                  putStrLn "------ The file already exists. Do you want to overwrite values? y / n ------"
                  override <- getLine
                  if override == "y" then do 
                  putStrLn "------ Path saved successfully ------"
                  menuENG titles notsignificants outputPath
                  else do putStrLn "------ Output path is now empty ------"
                          menuENG titles notsignificants ""
                else do 
                  putStrLn $ "The file doesn´t exist"
                  menuENG titles notsignificants ""
     "4" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|---------------Basic rotation has been done---------------|"
                putStrLn $ "------------------------------------------------------------"
                menuENG titles notsignificants outputPath
     "5" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|--------------Aligned rotation has been done--------------|"
                putStrLn $ "------------------------------------------------------------"
                menuENG titles notsignificants outputPath   
     "6" -> do
                putStrLn "Exiting..."
     _     -> do
                putStrLn $ "Unknown command ("++ comando ++"): '" ++ inpStr ++ "'" 
                menuENG titles notsignificants outputPath

menuESP :: [String] -> [String] -> String -> IO ()
menuESP titles notsignificants outputPath = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "                Bienvenido al menu en español              "
  putStrLn "-----------------------------------------------------------"
  putStrLn ">>> Por favor seleccione una de las siguientes opciones: "
  putStrLn "1- titulo           (Se leera la ruta ingresado)" 
  putStrLn "2- noSignificativo  (Se leera la ruta ingresado)" 
  putStrLn "3- fijarSalida      (Se almacenara el resultado en la ruta ingresado)"
  putStrLn "4- basica           (Se aplicara la rotacion basica y se guardara en la salida seleccionada)"
  putStrLn "5- alineada         (Se aplicara la rotacion alineada y se guardara en la salida seleccionada)"
  putStrLn "6- salir"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "1" -> do
                putStrLn ">>> Ingrese el nombre del archivo tipo titulo: "
                putStr ">> "
                nombreArchivo <- getLine
                -- mapM_ print titles
                exists1 <- doesFileExist nombreArchivo
                if exists1 then do
                  titles <- getLines nombreArchivo
                  putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
                  menuESP titles notsignificants outputPath
                else do
                  putStrLn $ "El archivo no existe"
                  menuESP [] notsignificants outputPath
     "2" -> do
                putStrLn ">>> Ingrese el nombre del archivo tipo no significativo: "
                putStr ">> "
                nombreArchivo <- getLine
                -- mapM_ print nosignificativos
                exists2 <- doesFileExist nombreArchivo
                if exists2 then do
                  nosignificativos <- getWords nombreArchivo
                  putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado" 
                  menuESP titles notsignificants outputPath
                else do
                  putStrLn $ "El archivo no existe"
                  menuESP titles [] outputPath
     "3" -> do
                putStrLn ">>> Nombre del archivo de salida: "
                putStr ">> "
                outputPath <- getLine
                -- Revisar si el archivo existe
                existe <- doesFileExist outputPath
                -- Si el archivo existe, imprime mensaje, sino escribir resultado en path de salida. Parametros: path, contenido
                if existe then do 
                  putStrLn "------ El archivo ya existe. Quiere sobreescribir valores? y/n ------"
                  override <- getLine
                  if override == "y" then do 
                  putStrLn "------ Ruta guardada correctamente ------"
                  menuESP titles notsignificants outputPath
                  else do putStrLn "------ La ruta de salida ahora está vacía ------"
                          menuESP titles notsignificants ""
                else do 
                  putStrLn $ "El archivo no existe"
                  menuESP titles notsignificants ""
     "4" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|------------Se ha realizado la rotación básica------------|"
                putStrLn $ "------------------------------------------------------------"
                menuESP titles notsignificants outputPath
     "5" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|-----------Se ha realizado la rotación alineada-----------|"
                putStrLn $ "------------------------------------------------------------"
                menuESP titles notsignificants outputPath   
     "6" -> do
                putStrLn "Saliendo..."
     _     -> do
                putStrLn $ "Comando Desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                menuESP titles notsignificants outputPath

mainloop :: Estado -> IO ()
mainloop estado = do
  putStrLn "-----------------------------------------------------------"
  putStrLn "| Bienvenido a la aplicacion PCEC/Welcome to the KWIC app |"
  putStrLn "-----------------------------------------------------------"
  putStrLn ">>> Elija el idioma deseado(esp)/Choose the desired language(eng)"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "esp" -> do
               putStrLn "---------------Cambiando al menu en español----------------"
               menuESP [] [] ""     
     "eng" -> do
               putStrLn "---------------Switching to the English menu---------------"
               menuENG [] [] ""
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado