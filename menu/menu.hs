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

type Estado = Map String Integer

main :: IO ()
main = do 
       mainloop (Map.fromList[])

menuENG :: [String] -> [String] -> String -> IO ()
menuENG titles notsignificants outputPath = do
  putStrLn "                Welcome to the english menu"
  putStrLn ">>> Please select one of the following options: "
  putStrLn "- titles  (The entered path will be read)" 
  putStrLn "- notSignificant  (The entered path will be read)" 
  putStrLn "- setOutput  (The entered path will be used to store results)"
  putStrLn "- basic (Basic rotation will be applied and stored in selected output)"
  putStrLn "- align (Align rotation will be applied and stored in selected output)"
  putStrLn "- exit"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "titles" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               titles <- getLines nombreArchivo
               -- mapM_ print titles
               putStrLn $ "File " ++ nombreArchivo ++ " was loaded"
               menuENG titles notsignificants outputPath
     "notSignificant" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               nosignificativos <- getWords nombreArchivo
               mapM_ print nosignificativos
               putStrLn $ "File " ++ nombreArchivo ++ " was loaded"
               menuENG titles nosignificativos outputPath
     "setOutput" -> do
               putStrLn ">>> Output file name: "
               putStr ">> "
               outputPath <- getLine
               -- Revisar si el archivo existe
               existe <- doesFileExist outputPath
               -- Si el archivo existe, imprime mensaje, sino escribir resultado en path de salida. Parametros: path, contenido
               if existe
                     then do putStrLn "------ El archivo existe. Quiere sobreescribir valores? y/n ------"
                             override <- getLine
                             if override == "y"
                               then do putStrLn "------ Path guardado existosamente ------"
                                       menuENG titles notsignificants outputPath
                               else do putStrLn "------ Output path is now empty ------"
                                       menuENG titles notsignificants ""
                     else do putStrLn "------ Path guardado existosamente ------"
                             menuENG titles notsignificants outputPath
     "basic" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               putStrLn $ "---- Basic rotation has been done ------"
               let outputString = (formatKwicOutput titles notsignificants)
               escribir outputPath outputString
               putStrLn $ "---- Basic rotation has been done ------"
               menuENG titles notsignificants outputPath
     "align" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Align rotation has been done"
               menuENG titles notsignificants outputPath   
     "exit" -> do
               putStrLn "Exiting..."
     _     -> do
               putStrLn $ "Unknown command ("++ comando ++"): '" ++ inpStr ++ "'" 
               menuENG titles notsignificants outputPath

menuESP :: Estado -> IO ()
menuESP estado = do
  putStrLn "                Bienvenido al menu en español"
  putStrLn ">>> Por favor seleccione una de las siguientes opciones: "
  putStrLn "- leer     (Se leera el archivo ingresado)" 
  putStrLn "- guardar  (Se guardara el archivo ingresado)"
  putStrLn "- basica   (Se aplicara la rotacion basica)"
  putStrLn "- alineada (Se aplicara la rotacion alineada)"
  putStrLn "- salir"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "leer" -> do
               putStrLn ">>> Nombre archivo entrada: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               menuESP estado
     "guardar" -> do
               putStrLn ">>> Nombre archivo salida: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue guardado"
               menuESP estado
     "basica" -> do
               putStrLn ">>> Nombre archivo entrada: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Se ha hecho la rotación básica"
               menuESP estado
     "alineada" -> do
               putStrLn ">>> Nombre archivo entrada: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Se ha hecho la rotación alineada"
               menuESP estado   
     "salir" -> do
               putStrLn "Saliendo..."
     _     -> do
               putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
               menuESP estado

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
               menuESP (Map.fromList[])     
     "eng" -> do
               putStrLn "---------------Switching to the English menu---------------"
               menuENG [] [] ""
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- funciones necesarias
