-- menu del proyecto III
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).

import Prelude hiding (null, lookup, map, filter)
import Data.Char
import Data.List (sort,map, nub, concat)
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

sigRotations xs notSignificants = [ drop i xs ++ take i xs | i <- [0 .. n], not ((lowercase (xs!!i)) `elem` notSignificants) ]
                  where n = (length xs) - 1

putSpaces xss = tail (concat (map (' ':) xss))

sep xs = init xs ++ [last xs ++ " ><"]

kwic titles notSignificants = map putSpaces (sigRotations (sep (toWords titles)) notSignificants)

addNewLineOutputs lista = map (\x -> concat(x : [" \n "])) lista

-- Cargar palabras de archivo de texto en lista.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)

-- Cargar lineas de archivo de texto en lista.
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)

generateAllKwic ts ns = map (`kwic` ns) ts
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
  putStrLn "                Welcome to the english menu"
  putStrLn ">>> Please select one of the following options: "
  putStrLn "1- titles  (The entered path will be read)" 
  putStrLn "2- notSignificant  (The entered path will be read)" 
  putStrLn "3- setOutput  (The entered path will be used to store results)"
  putStrLn "4- basic (Basic rotation will be applied and stored in selected output)"
  putStrLn "5- align (Align rotation will be applied and stored in selected output)"
  putStrLn "6- exit"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "1" -> do
                putStrLn ">>> Enter the title type file name: "
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
                putStrLn ">>> Enter the not significant type file name: "
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
                  putStrLn "------ El archivo existe. Quiere sobreescribir valores? y/n ------"
                  override <- getLine
                  if override == "y" then do 
                  putStrLn "------ Path guardado existosamente ------"
                  menuENG titles notsignificants outputPath
                  else do putStrLn "------ Output path is now empty ------"
                          menuENG titles notsignificants ""
                else do 
                  putStrLn $ "The file doesn´t exist"
                  menuENG titles notsignificants ""
     "4" -> do
                if (map.null titles || length notsignificants == 0) then do
                  putStrLn $ "---- Some data is missing ------"
                  menuENG titles notsignificants outputPath
                else do
                  putStrLn ">>> Input file name: "
                  putStr ">> "
                  -- print ((formatKwicList titles notsignificants))
                  let outputString = (formatKwicOutput titles notsignificants)
                  escribir outputPath outputString
                  putStrLn $ "---- Basic rotation has been done ------"
                  menuENG titles notsignificants outputPath
     "5" -> do
                putStrLn ">>> Input file name: "
                putStr ">> "
                nombreArchivo <- getLine
                putStrLn $ "Align rotation has been done"
                menuENG titles notsignificants outputPath   
     "6" -> do
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
