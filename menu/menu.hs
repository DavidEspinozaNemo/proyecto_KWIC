-- menu del proyecto III
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).

import Prelude hiding (null, lookup, map, filter)
import Data.Char
import Data.List (sort,map)
import System.IO

type Estado = Map String Integer

main :: IO ()
main = do 
       mainloop (Map.fromList[])

menuENG :: Estado -> IO ()
menuENG estado = do
  putStrLn "                Welcome to the english menu"
  putStrLn ">>> Please select one of the following options: "
  putStrLn "- read  (The entered file will be read)" 
  putStrLn "- save  (The entered file will be save)"
  putStrLn "- basic (Basic rotation will be applied)"
  putStrLn "- align (Align rotation will be applied)"
  putStrLn "- exit"
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "read" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "File " ++ nombreArchivo ++ " was load"
               menuENG estado
     "save" -> do
               putStrLn ">>> Output file name: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "File " ++ nombreArchivo ++ " was saved"
               menuENG estado
     "basic" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Basic rotation has been done"
               menuENG estado
     "align" -> do
               putStrLn ">>> Input file name: "
               putStr ">> "
               nombreArchivo <- getLine
               putStrLn $ "Align rotation has been done"
               menuENG estado   
     "exit" -> do
               putStrLn "Exiting..."
     _     -> do
               putStrLn $ "Unknown command ("++ comando ++"): '" ++ inpStr ++ "'" 
               menuENG estado

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
               menuENG (Map.fromList[])
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- funciones necesarias