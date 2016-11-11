module Main where

import Instrucciones
import IO

--ejecutarPrograma f es un lienzo con las instrucciones escritas en el archivo f ejecutadas
ejecutarPrograma :: FilePath -> IO Lienzo
ejecutarPrograma fp = do 
                         file <- openFile fp ReadMode
                         line <- hGetLine file
                         let (dim,r) = head $lex line 
                             (filas, r1) = head  ( lex r )
                             (cols, r2) = head  (lex r1) in 
                             do
                                cmd <- parseFile file
                                return $ interpretarBatch (read filas :: Integer, read cols :: Integer) cmd

--parseFile f es una lista de las instrucciones escritas en el archivo f                     
parseFile file = do 
                    line <- hGetLine file
                    if (head line) == 'e' 
                        then return []
                        else 
                             do resto <- (parseFile file) 
                                return ((readInstruccion line)++resto)


--exportarLienzo l f es un archivo de direccion f con el lienzo l escrito 
exportarLienzo :: Lienzo -> FilePath -> IO ()
exportarLienzo l fp = do
                         file <- openFile fp WriteMode
                         hPutStr file (show l)
                         hClose file

--readInstruccion a una instruccion interpretada del string a
--readInstruccion :: (Read a) => ReadS ([a])
readInstruccion ('c':'o':'l':'o':'r':xs) = [EstablecerColor (head color) | (color, r) <- lex xs]

readInstruccion ('g':'e':'t':xs) = [ObtenerColor (fila,col) | (fila, r) <- reads xs,
                                                           (col, t) <- reads r ]  

readInstruccion ('p':'o':'i':'n':'t':xs) = [DibujarPunto (fila,col) | (fila, r) <- reads xs,
                                                         (col, t) <- reads r ]  

readInstruccion ('l':'i':'n':'e':xs) = [(DibujarLinea (f1,c1) (f2,c2)) | (f1, a) <- reads xs,
                                                               (c1, b) <- reads a,
                                                               (f2, c) <- reads b,
                                                               (c2, d) <- reads c]

readInstruccion ('c':'i':'r':'c':'l':'e':xs) = [DibujarCirculo (f1,c1) r | (f1, a) <- reads xs,
                                                               (c1, b) <- reads a,
                                                               (r, c) <- reads b ]

readInstruccion ('f':'i':'l':'l':xs) = [Llenar (fila,col) | (fila, r) <- reads xs,
                                                  (col, t) <- reads r ] 

readInstruccion ('c':'u':'r':'v':'e':xs) = [DibujarCurva (f1,c1) r  ang | (f1, a) <- reads xs,
                                                               (c1, b) <- reads a,
                                                               (r, c) <- reads b ,
                                                               (ang, d) <- reads c]

readInstruccion ('r':'p':'o':'l':'y':'g':'o':'n':xs) = [DibujarPoligonoRegular (f1,c1) r n | (f1, a) <- reads xs,
                                                                     (c1, b) <- reads a,
                                                                     (r, c) <- reads b ,
                                                                     (n, d) <- reads c]

readInstruccion ('c':'p':'o':'l':'y':'g':'o':'n':xs) = [DibujarPoligono (getVertices num r) False |  (num, r) <- reads xs]

readInstruccion ('p':'o':'l':'y':'g':'o':'n':xs) = [DibujarPoligono (getVertices num r) True |  (num, r) <- reads xs]

readInstruccion ('t':'r':'i':'a':'n':'g':'u':'l':'a':'r':'i':'z':'e':xs) = [Triangularizar (getVertices num r) |  (num, r) <- reads xs]


readInstruccion ('e':'n':'d':xs) = []



getVertices :: Integer -> String -> [Posicion] 
getVertices num s | num == 0 = []
                  | otherwise = (f,c):getVertices (num-1) rest
                  where (f, t) = head (reads s)
                        (c, rest) = head (reads t)


