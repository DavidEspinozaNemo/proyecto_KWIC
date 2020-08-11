
import System.Directory

-- Cargar palabras de archivo de texto en lista.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)

-- Cargar lineas de archivo de texto en lista.
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)

-- Escribir contenido en path
escribir :: FilePath -> String -> IO()
escribir path contenido = writeFile path (contenido)

main = do

    -- Leer titulos. Parametros: path
    titulos <- getLines "/home/jason/Escritorio/Haskell/reader/titulos.txt"
    mapM_ print titulos
    putStrLn "------Titulos leidos------"

    -- Leer palabras no significativas. Parametros: path
    nosignificativos <- getWords "/home/jason/Escritorio/Haskell/reader/nosignificativas.txt"
    mapM_ print nosignificativos
    putStrLn "------NoSignificativos leidos------"
   
    -- Revisar si el archivo existe
    existe <- doesFileExist "/home/jason/Escritorio/Haskell/reader/pruebaescritura.txt"

    -- Si el archivo existe, imprime mensaje, sino escribir resultado en path de salida. Parametros: path, contenido
    if existe
        then putStrLn "------El archivo existe------"
        else escribir "/home/jason/Escritorio/Haskell/reader/pruebaescritura.txt" "Hasasasola \nEsta es la segunda linea"
