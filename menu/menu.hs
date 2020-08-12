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

menuEjemplo :: Estado -> IO ()
menuEjemplo estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of    
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Todos son desconocidos ("++ comando ++"): '" ++ inpStr ++ "'" 
                 menuEjemplo estado

mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "leer" -> do
               putStrLn ">>> Nombre archivo entrada: "
               nombreArchivo <- getLine
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               mainloop estado
-- cuando sucede que debemos cambiar, debemos hacer un nuevo estado y pasarlo como parametro
     "guardar" -> do
                    putStrLn ">>> Nombre archivo salida: "
                    nombreArchivo <- getLine
                    putStrLn $ "Archivo " ++ nombreArchivo ++ " fue guardado"
                    mainloop estado  
     "nuevoMenu" -> do
                      menuEjemplo (Map.fromList[])
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- funciones necesarias
