module Instrucciones (Instruccion (..), Magnitud, Posicion, Lienzo, interpretarComando, interpretarBatch, lienzoVacio) where

import Lienzo
import List

data Instruccion = Limpiar
		   | EstablecerColor Char
		   | ObtenerColor Posicion
		   | DibujarPunto Posicion
		   | DibujarLinea Posicion Posicion
		   | DibujarCirculo Posicion Magnitud
		   | Llenar Posicion
		   | DibujarCurva Posicion Magnitud Magnitud
		   | DibujarPoligono [Posicion] Bool
		   | DibujarPoligonoRegular Posicion Integer Integer 
		   | Triangularizar [Posicion]
		 
type Magnitud = Integer

--interpretarComando (l,c) i es una tupla que contiene el lienzo l luego de la ejecucion de la instruccion i y el caracter activo
interpretarComando :: (Lienzo, Char) -> Instruccion -> (Lienzo, Char)
interpretarComando (l,c) Limpiar = (lienzoVacio (getDim l), c)

interpretarComando (l,c) (EstablecerColor d) = (l,d)

interpretarComando (l,c) (ObtenerColor pos) = (l, obtenerColor l pos)

interpretarComando (l,c) (DibujarPunto pos) = ((dibujarPunto l pos c), c)

interpretarComando (l,c) (DibujarLinea (li,ci) (lf,cf)) = (dibujarLineaPuntos l (li,ci) (lf,cf) c, c)

interpretarComando (l,c) (DibujarCirculo pos radio) = ((dibujarCirculo l pos radio c), c)

interpretarComando (l,c) (Llenar pos) = ((llenar l pos c), c)

interpretarComando (l,c) (DibujarCurva pos radio arco) = (dibujarMiCirculo l pos radio 180 (fromInteger (arco+180)) c, c)

interpretarComando (l,c) (DibujarPoligonoRegular pos lados long) 
                  | lados < 3 = (l,c)
                  | otherwise = ( dibujarPoligonoPuntos l pos (verticesPoligonoReg lados long [pos]) c  ,  c)

interpretarComando (l,c) (DibujarPoligono pos cruces) | cruces = (dibujarPoligonoPuntos l (head pos) pos c, c)
                                                      | otherwise = (dibujarPoligonoPuntos l puntoIzq orderedPoints c, c)
                                                      where orderedPoints = puntoIzq:(ordenarPuntosAngulo (puntoIzq) (filter (/=puntoIzq) pos)) 
                                                            puntoIzq = getLeftmostPoint pos (head pos)

interpretarComando (l,c) (Triangularizar pos) 
                  | length pos > 2 = ((triangularizar l pos c),c)
                  | otherwise = (l,c)

--triangularizar l v c dibuja con el caracter c en el lienzo l la secuencia de triangulos formada por la lista de vertices v
triangularizar :: Lienzo -> [Posicion] -> Char -> Lienzo
triangularizar l pos c | length pos > 2 = triangularizar (dibujarLineaPuntos (dibujarLineaPuntos l (pos!!0) (pos!!1) c) (pos!!0) (pos!!2) c) (tail pos) c
                       | length pos == 2 = dibujarLineaPuntos l (pos!!0) (pos!!1) c

--verticesPoligonoReg i j v es una lista ordenada de los puntos que forman el poligono regular de i lados de longitud j 
verticesPoligonoReg :: Integer -> Integer -> [Posicion] -> [Posicion]
verticesPoligonoReg lados long vert | count + 1== (fromInteger lados) = reverse vert
                                    | otherwise = verticesPoligonoReg lados long (((fst lastV)+step_y, (snd lastV)+step_x):vert)
                                    where count = (length vert) - 1
                                          ang = 2*pi/(fromIntegral lados)
                                          alfa = (fromIntegral count)*(ang)
                                          step_y = round ((fromIntegral long)*(sin alfa))
                                          step_x = round ((fromIntegral long)*(cos alfa))
                                          lastV = head vert


--dibujarPoligonoPuntos l p v c dibuja con el caracter c en el lienzo l el poligono formado por los puntos de la lista v 
dibujarPoligonoPuntos :: Lienzo -> Posicion -> [Posicion] -> Char -> Lienzo
dibujarPoligonoPuntos l start (p1:p2:xs) c = dibujarPoligonoPuntos (dibujarLineaPuntos l p1 p2 c)  start (p2:xs) c
dibujarPoligonoPuntos l start (p1:[]) c = dibujarLineaPuntos l p1 start c
dibujarPoligonoPuntos l start [] c = l

                        
--interpretarBatch (i,j) i es un lienzo de dimensiones i,j con las instrucciones de la lista i ejecutadas
interpretarBatch :: (Integer, Integer) -> [Instruccion] -> Lienzo
interpretarBatch (alto, ancho) [] = lienzoVacio (alto, ancho)
interpretarBatch (alto, ancho) ins = interpretarMiBatch (lienzoVacio (alto, ancho), '.') ins

interpretarMiBatch :: (Lienzo,Char) -> [Instruccion] -> Lienzo
interpretarMiBatch (l,c) (x:xs) = interpretarMiBatch (interpretarComando (l,c) x) xs
interpretarMiBatch (l,c) [] = l


-- getLeftmostPoint v p es el vertice mas izquierdo de la lista v
getLeftmostPoint :: [Posicion] -> Posicion -> Posicion
getLeftmostPoint ((x,y):xs) (a,b) | b <= y = getLeftmostPoint xs (a,b)
                                  | otherwise = getLeftmostPoint xs (x,y)
getLeftmostPoint [] (a,b) = (a,b)   

--ordenarPuntosAngulo p v es una lista ordenada con relacion a p de los puntos de la lista v 
ordenarPuntosAngulo :: Posicion -> [Posicion] -> [Posicion]
ordenarPuntosAngulo ini puntos = sortBy compAng puntos
                                 where compAng a b 
                                                   | (angulo ini a) >= (angulo ini b) = GT
                                                   | otherwise = LT
                                       angulo (x,y) (s,t) = atan (fromRational (fromInteger (s-x))/(fromInteger (y-t)))

