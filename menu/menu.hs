{-# LANGUAGE FlexibleContexts #-}

-- menu del proyecto III
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).

import Prelude hiding (null, lookup, map, filter)
import Data.Char
import Data.List (sort,map, nub, concat, filter, sortBy, findIndices)
import System.IO
import System.Directory
import Data.Typeable


-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float   

-- Codigo profe
toWords :: [Char] -> [[Char]] 
toWords [] = []
toWords (x:xs) | x == ' '  = toWords (dropWhile (' ' ==) xs)
               | otherwise = (x:takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)

lowercase = map toLower
uppercase = map toUpper
lowercases = map lowercase

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Generar rotaciones para arreglo de strings. Recibe arreglo de strings y retorna lista arreglos con rotaciones
sigRotations xs = [ drop i xs ++ take i xs | i <- [0 .. n]]
                  where n = (length xs) - 1


-- Revisar si una palabra es toda mayuscula
hasLower palabra = do let minusculas = filter (\x -> x `elem` ['a'..'z']) palabra
                      ((length minusculas) > 0)


-- Buscar indices que contienen palabras en mayuscula
buscarIndicesMayusculas titleArray = [ i | i <- [0 .. n] , not (hasLower(titleArray!!i))]
                  where n = (length titleArray) - 1

-- Extraer elemento encontrado
extraerIndiceSignificativo titleArray = (buscarIndicesMayusculas titleArray)!!0

-- Generar rotaciones alineadas para arreglo de strings a partir de indice 1 (es necesario agregar rotacion para indice 0 y n -1 aparte).
-- Recibe arreglo de strings y retorna lista arreglos con rotaciones
sigAlineadas xs = [ ((slice 0 (i-1) xs)) ++ ([uppercase(xs!!i)]) ++ ((slice (i+1) ((length xs) - 1)) xs) | i <- [1 .. n]]
                  where n = (length xs) - 2

-- Agrega espacios a elementos de una lista y los concatena
putSpaces xss = tail (concat (map (' ':) xss))

-- Agregar separador al final de una oracion
sep xs = init xs ++ [last xs ++ " ><"]

-- Filtra listas que tienen como primer elemento una palabra no significativa
getSignificants titles ns = filter (\x -> not (lowercase(head x) `elem` ns)) titles

-- Filtra los titulos que poseen indexada una palabra significativa
getAlignedSignificants titles ns = filter (\x -> not (lowercase(x!!(extraerIndiceSignificativo x)) `elem` ns)) titles

-- Generar rotaciones a partir de un titulo y agregar espacios a cada elemento
kwic title notSignificants = do let titleRotations = sigRotations (sep (toWords title))
                                let filteredRotations = getSignificants titleRotations notSignificants
                                map putSpaces (filteredRotations)


-- [["SUPER", "man"], ["super", "MAN"], ["SOME", "boy"] ["some", "BOY"]]
-- Filtra los titulos que poseen indexada una palabra significativa
quicksort :: [[String]] -> [[String]]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, (a!!(extraerIndiceSignificativo a)) <= (x!!(extraerIndiceSignificativo x))]
      biggerSorted = quicksort [a | a <- xs, (a!!(extraerIndiceSignificativo a)) > (x!!(extraerIndiceSignificativo x))]
  in  smallerSorted ++ [x] ++ biggerSorted

-- Almacenar listas de tamanos de las listas de sub-titulos
generateSizes xs = [length (xs!!i)| i <- [0 .. n]]
                where n = (length xs) - 1

-- Generar sub listas que incluyen solo el inicio de los titulos hasta su respectiva palabra significativa
getStartToIndex xs = [ slice 0 ((extraerIndiceSignificativo a)-1) a | a <- xs]

generateSpaces spaces number = do 
                         if length spaces == (number)
                           then spaces
                         else do
                           generateSpaces (spaces ++ " ") number 

generateConsoleSpaces spaces number = do 
                         if length spaces == (number)
                           then spaces
                         else do
                           generateConsoleSpaces (spaces ++ " ") number 

-- Obtener tamano de titulo mas grande
getLongestWords :: [[String]] -> Int
getLongestWords titles = do
                            -- Generar sub listas [["rotacion", "UNO"],["ROTACION","uno"],["rotacion", "DOS"], ["ROTACION", "dos"]]
                            let startToIndex = getStartToIndex titles
                            -- Agregar espacios a rotacions [["rotacion ", "UNO "],["ROTACION ","uno "],["rotacion ", "DOS "], ["ROTACION ", "dos "]]
                            let withSpaces = map putSpaces startToIndex
                            -- Generar lista con distancias hasta palabra significativa para las rotaciones y obtener distancia mayor
                            let sizes = generateSizes withSpaces
                            let maximo = maximum (sizes)

                            maximum (sizes)
                            
-- Generar rotaciones a partir de un titulo y agregar espacios a cada elemento
kwicAlineado title notSignificants = do 
                                        let titleArray = toWords (lowercase(title))
                                        let firstWord = [[(uppercase(titleArray!!0))] ++ ((slice 1 ((length titleArray) - 1)) titleArray)]
                                        let titleRotations = merge (sigAlineadas (titleArray))  firstWord
                                        let lastRotation = merge titleRotations [ (slice 0 ((length titleArray)-2) titleArray) ++ [uppercase(titleArray!!(length titleArray - 1))]]
                                        getAlignedSignificants lastRotation notSignificants

-- Agrega un salto de linea a strings de una lista
addNewLineOutputs lista = map (\x -> concat(x : [" \n "])) lista

-- Imprime rotaciones para titulos
printRotations ts ns = map sigRotations (map sep (map toWords ts))

findCaps palabra = ((findIndices (`elem` ['A'..'Z']) palabra)!!0)

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

-- For every element in a list of alined titles, generate rotations and add spaces and concatenate the words in each rotation
generateAllKwicAlined2 ts ns =  do 
                                let todasRotaciones = concat (map (`kwicAlineado` ns) ts)
                                let ordenadas = quicksort todasRotaciones
                                let oraciones = map putSpaces ordenadas
                                let indices = map findCaps oraciones
                                let mayor = (sort (indices))!!((length indices)-1)
                                let nuevosTitulos = [ (generateSpaces "" (mayor-indices!!i)) ++ (oraciones!!i) | i <- [0 .. n]]
                                                  where n = (length oraciones) - 1
                                nuevosTitulos

-- For every element in a list of alined titles, generate rotations and add spaces and concatenate the words in each rotation
generateKwicForConsole ts ns =  do 
                                let todasRotaciones = concat (map (`kwicAlineado` ns) ts)
                                let ordenadas = quicksort todasRotaciones
                                let oraciones = map putSpaces ordenadas
                                let indices = map findCaps oraciones
                                let mayor = (sort (indices))!!((length indices)-1)
                                let nuevosTitulos = [ (generateConsoleSpaces "" (mayor-indices!!i)) ++ (oraciones!!i) | i <- [0 .. n]]
                                                  where n = (length oraciones) - 1
                                nuevosTitulos

-- For every rotation, add a new line
formatKwicOutput ts ns = concat(addNewLineOutputs (nub(sort(concat(generateAllKwic ts ns)))))

-- For every rotation, add a new line
formatKwicAlinedOutput ts ns = concat(addNewLineOutputs(generateAllKwicAlined2 ts ns))

-- Escribir contenido en path
escribir :: FilePath -> String -> IO()
escribir path contenido = writeFile path (contenido)

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
                -- if (checkCaps "hola") then do
                --  putStrLn "yes"
                -- else do
                --  putStrLn "no"

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
                  menuENG titles nosignificativos outputPath
                else do
                  putStrLn $ "The file doesn´t exist"
                  menuENG titles [] outputPath
     "3" -> do
                putStrLn ">>> Output file name: "
                putStr ">> "
                userPath <- getLine
                -- Revisar si el archivo existe
                existe <- doesFileExist userPath
                -- Si el archivo existe, imprime mensaje, sino escribir resultado en path de salida. Parametros: path, contenido
                if existe then do 
                  putStrLn "------ The file already exists. Do you want to overwrite values? y / n ------"
                  override <- getLine
                  if override == "y" then do 
                  putStrLn "------Output path saved successfully ------"
                  menuENG titles notsignificants outputPath
                  else do putStrLn "------ Output path is now empty ------"
                          menuENG titles notsignificants ""
                else do 
                  putStrLn $ "Output path saved successfully"
                  menuENG titles notsignificants userPath
     "4" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|---------------Basic rotation has been done---------------|"
                putStrLn $ "------------------------------------------------------------"
                menuENG titles notsignificants outputPath
     "5" -> do
                let outputString = (formatKwicAlinedOutput titles notsignificants)
                escribir outputPath (" " ++ outputString)
                mapM_ putStrLn (["\n"] ++ generateKwicForConsole titles notsignificants)
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
                  menuESP titles nosignificativos outputPath
                else do
                  putStrLn $ "El archivo no existe"
                  menuESP titles [] outputPath
     "3" -> do
                putStrLn ">>> Nombre del archivo de salida: "
                putStr ">> "
                userPath <- getLine
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
                  putStrLn $ "El path de salido se ha guardado correctamente"
                  menuESP titles notsignificants userPath
     "4" -> do
                -- print ((formatKwicList titles notsignificants))
                let outputString = (formatKwicOutput titles notsignificants)
                escribir outputPath outputString
                putStrLn $ "------------------------------------------------------------"
                putStrLn $ "|------------Se ha realizado la rotación básica------------|"
                putStrLn $ "------------------------------------------------------------"
                menuESP titles notsignificants outputPath
     "5" -> do
                let outputString = (formatKwicAlinedOutput titles notsignificants)
                escribir outputPath (" " ++ outputString)
                mapM_ putStrLn (["\n"] ++ generateKwicForConsole titles notsignificants)
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