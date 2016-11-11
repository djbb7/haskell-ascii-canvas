module Lienzo (Lienzo, Posicion, lienzoVacio, dibujarPunto, dibujarLinea, dibujarLineaPuntos, dibujarCirculo, dibujarMiCirculo, llenar, obtenerColor, getDim) where

data Lienzo = MkLienzo (Integer, Integer) [[Char]] 

type Posicion = (Integer, Integer)

instance Show (Lienzo) where
	show(MkLienzo (h,w) m) = showsLienzo m

--showsLienzo m es un string que representa al lienzo m 
showsLienzo m | m==[] = ""
	      | otherwise = showLine (head m) ++'\n': showsLienzo (tail m)

showLine fila = foldr (\elem rest->elem:' ':rest) [] fila

--lienzoValido l verifica si l es un lienzo valido
lienzoValido :: Lienzo -> Bool
lienzoValido (MkLienzo (alt, ancho) matriz) = (length matriz == h) && (revisaFila w matriz)
	     	       	     	    	    where revisaFila a m |  m==[] = True
					    	  	       	 | otherwise = a==length(head m) && (revisaFila a (tail m))
						  w = fromInteger ancho
						  h = fromInteger alt

--lienzoVacio (h,w) es un lienzo vacio de altura h y ancho w
lienzoVacio :: (Integer, Integer) -> Lienzo
lienzoVacio (altura, ancho) | altura > 0 && ancho > 0 = MkLienzo (altura, ancho) (replicate (fromInteger altura) (replicate (fromInteger ancho) ' '))
	    	     	    | otherwise = error "La altura y ancho del lienzo deben ser positivos.\n"


--dibujarPunto l (i,j) c dibuja en las coordenadas i,j del lienzo l el caracter c
dibujarPunto :: Lienzo -> Posicion -> Char -> Lienzo
dibujarPunto (MkLienzo (h,w) m) (f, clm) c | f>=h || clm>=w || f<0 || clm < 0 = MkLienzo (h,w) m 
	          	        	   | otherwise = MkLienzo (alt, ancho) (insertarPunto c fila col m) 
				  	   where alt = fromInteger h
					         ancho = fromInteger w
						 fila = fromInteger f
						 col = fromInteger clm
						 insertarPunto c fila col m = (take fila m) 
						 	       ++ ((take col (m!!fila)) ++ c :(drop (col+1) (m!!fila))) 
							       : (drop (fila+1) m)

--dibujarLinea l (i,j) a len c dibuja en el lienzo l una linea de caracteres c a partir de la posicion i,j formando un angulo a 
dibujarLinea :: Lienzo -> Posicion -> Float -> Integer -> Char -> Lienzo
dibujarLinea l p ang long c = dibujarMiLinea l p step_x step_y 0 long c
				where step_x = cos alfa
				      step_y = sin alfa
				      alfa = (fromInteger ( mod (round ang) 360) )*pi/180 

--dibujarLineaPuntos l (i,j) (a,b) c dibuja en el lienzo l una linea de caracteres c desde la posicion i,j hasta la posicion a,b 
dibujarLineaPuntos :: Lienzo -> Posicion -> Posicion -> Char -> Lienzo
dibujarLineaPuntos  l (li,ci) (lf,cf) c | cf >= ci = dibujarLinea l (li,ci) angulo longitud c
                                        | otherwise = dibujarLinea l (lf,cf) angulo longitud c
                                        where longitud = round ( sqrt ( (fromInteger (cf-ci))**2 + (fromInteger (lf-li))**2 ) )  
                                              angulo = ( atan ( (fromInteger (li-lf)) / (fromInteger (cf-ci))) )*180/pi


dibujarMiLinea :: Lienzo -> Posicion -> Float -> Float -> Integer -> Integer -> Char -> Lienzo
dibujarMiLinea l (i,j) step_x step_y num_pasos long c 
		| num_pasos > long = l
		| otherwise = dibujarMiLinea (dibujarPunto l (round ((fromInteger i)-step_y*np), round((fromInteger j)+step_x*np)) c) (i,j) step_x step_y (num_pasos +1) long c
		where np = fromInteger num_pasos

--llenar l (i,j) c colorea con c todo el espacio contiguo a la posicion (i,j)
llenar :: Lienzo -> Posicion -> Char -> Lienzo
llenar l p c = flood_fill l p (obtenerColor l p) c


flood_fill :: Lienzo -> Posicion -> Char -> Char -> Lienzo
flood_fill l (fila,col) ca cd | (not (verificaCoord l (fila,col))) 
                                 || ca /= (obtenerColor l (fila, col)) 
                                 || cd == (obtenerColor l (fila, col)) = l
                              | otherwise = l4
                              where l1 = flood_fill (dibujarPunto l (fila,col) cd) (fila, col+1) ca cd
                                    l2 = flood_fill l1 (fila, col-1) ca cd
                                    l3 = flood_fill l2 (fila+1, col) ca cd
                                    l4 = flood_fill l3 (fila-1, col) ca cd

--verificaCoord l (i,j) verifica si (i,j) es una posicion valida en l 
verificaCoord :: Lienzo -> Posicion -> Bool
verificaCoord (MkLienzo (h,w) m) (f, clm) =  f<h && clm<w && f>=0 && clm>=0 

--dibujarCircul l (i,j) r c dibuja un circulo en l de caracteres c de centro i,j con radio r
dibujarCirculo :: Lienzo -> Posicion -> Integer -> Char -> Lienzo
dibujarCirculo l p r c = dibujarMiCirculo l p r 0 360 c

dibujarMiCirculo :: Lienzo -> Posicion -> Integer -> Float-> Float -> Char -> Lienzo
dibujarMiCirculo l (fila,col) r rec arco c 
		   | rec > arco = l
		   | otherwise = dibujarMiCirculo ( dibujarPunto l (fila-step_x,col+step_y)   c ) (fila, col) r (rec+1) arco c
		   where alfa = rec*pi/180
		   	 step_x = round ((fromInteger r)*(sin alfa))
		   	 step_y = round ((fromInteger r)*(cos alfa))

--obtenerColor l (i,j) es el caracter que este en la posicion (i,j)
obtenerColor :: Lienzo -> Posicion -> Char
obtenerColor (MkLienzo (h,w) matriz) (alt,anch) | not (lienzoValido (MkLienzo (h,w) matriz)) = error "El lienzo no es valido"
	     	       	     	     		| alt>=h || anch>=w = error "La posicion esta afuera del lienzo"
	     	       	     	     		| otherwise  = matriz!!(fromInteger alt)!!(fromInteger anch)

--getDim l son las dimensiones del lienzo l
getDim :: Lienzo -> (Integer, Integer)
getDim (MkLienzo (h,w) m) = (h,w)


