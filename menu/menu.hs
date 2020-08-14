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

kwic notSignificants = nub . sort . concat . map pre
       where pre ys = map putSpaces (sigRotations (sep (toWords ys)) notSignificants)

printKwic ts ns = map putStrLn (kwic ts ns)

writeResults output ts ns = map escribir (kwic ts ns)

-- Cargar palabras de archivo de texto en lista.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)

-- Cargar lineas de archivo de texto en lista.
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)

printWords ts = map toWords ts
printSep ts ns = (map sep (map toWords ts))
printRotations ts ns = map (`sigRotations` ns) (map sep (map toWords ts))

-- oraciones[ oracion[rotacion["wergwergwerg","gertgertgetrg"] ,  rotacion["wergwergwerg","gertgertgetrg"] ]]
--printSpaces ts ns = map putSpaces ((printRotations ts ns) !! 0)


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
               mapM_ print titles
               putStrLn $ "File " ++ nombreArchivo ++ " was load"
               menuENG titles notsignificants outputPath
     "notSignificant" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               nosignificativos <- getWords nombreArchivo
               mapM_ print nosignificativos
               putStrLn $ "File " ++ nombreArchivo ++ " was load"
               menuENG titles notsignificants outputPath
     "setOutput" -> do
               putStrLn ">>> Output file name: "
               putStr ">> "
               outputPath <- getLine
               putStrLn $ "File " ++ outputPath ++ " was saved"
               menuENG titles notsignificants outputPath
     "basic" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               putStrLn $ "---- Basic rotation has been done ------"
               print ((printRotations titles notsignificants))
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
