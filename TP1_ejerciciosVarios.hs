Punto 1) Generar la cadena masVocalesQueConsonantes que recibe una cadena y  devuelve True o False

esVocal caracter = elem caracter "aeiou"

esConsonante caracter = not (esVocal caracter)

filtrarConsonantes cadena = filter (esConsonante) cadena

filtrarVocales cadena = filter (esVocal) cadena

cuentaVocales cadena = length (filtrarVocales cadena)

cuentaConsonantes cadena = length (filtrarConsonantes cadena)


masVocalesQueConsonantes cadena = (cuentaVocales cadena) > (cuentaConsonantes cadena) 

masVocalesQueConsonantes2  cadena 

			| (cuentaVocales cadena) > (cuentaConsonantes cadena) = "Mas"
	   		| (cuentaVocales cadena) < (cuentaConsonantes cadena) = "Menos"
	   		| otherwise = "Igual"


Punto 2) Desarrollar funcion esPrimo que recibe un numero y devuelve True o False

listaNumerosAEvaluar x = [ x | x<-[2..x-1]]

divisionPorNumero x = map (mod x) (listaNumerosAEvaluar x)

esPrimo x = not (any (== 0) (divisionPorNumero x))

Punto 3) Desarrollar la funcion mensajito que recibe un conjunto de cadenas (["hola", "q", "kc"]) y retorna True o False si la cantidad de cadenas con longitud menor a 3 es mayor a la cantidad de cadenas mayor a 3 de longitud

lengthStrings lista_cadenas = map (length) lista_cadenas 

filterSmallStrings lista_lengths = filter (<=3) lista_lengths

filterBigStrings lista_lengths = filter (>3) lista_lengths

mensajito lista_cadenas | length (filterSmallStrings (lengthStrings lista_cadenas)) > length (filterBigStrings (lengthStrings lista_cadenas))  = True
						            |  length (filterSmallStrings (lengthStrings lista_cadenas)) < length (filterBigStrings (lengthStrings lista_cadenas))  = False
						
mensajitoOK lista_cadenas | length (filterSmallStrings (lengthStrings lista_cadenas)) > length (filterBigStrings (lengthStrings lista_cadenas))  = "Es mensajito"
						              |  length (filterSmallStrings (lengthStrings lista_cadenas)) < length (filterBigStrings (lengthStrings lista_cadenas))  = "No es mensajito"
						              | otherwise = "Misma cantidad"

Punto 4) Desarrollar la funcion cuentaPalabrasRusasEnTexto. Una palabra es considerada rusa si tiene los caracteres "k" e "y" y no tiene mas de dos vocales
 palabraRusa cadena = (any (==k) cadena) && (any (==y) cadena)  

cuentasPalabrasRusas lista = length (filter palabraRusa lista)


!!!!!!!!!!clase martes


tamano           lista = map (length) lista
listamenor       lista b = filter (<=b) (tamano lista)
listamayor       lista b = filter (>b) (tamano lista)
tamanolistamenor lista b = length (listamenor lista b)
tamanolistamayor lista b = length (listamayor lista b)
mensajito        lista b = (tamanolistamenor lista b) > (tamanolistamayor lista b)



distancia (x,y) = sqrt ( x*x + y*y )

cantidadDistancia lista = map (distancia) lista
filtradoradio   lista b = filter (<=b) (cantidadDistancia lista)
filtradonoradio lista b = filter (>b) (cantidadDistancia lista)
tamañoradio     lista b = length (filtradoradio lista b ) 
tamañonoradio   lista b = length (filtradonoradio lista b )
centrada        lista b = (tamañoradio lista b ) > (tamañonoradio lista b )

centrada2 lista b = laMayoriaCumple distancia lista b
mensajito2 lista b = laMayoriaCumple tamano lista b


laMayoriaCumple funcion lista valor = funcionMenor funcion lista valor > funcionMayor funcion lista valor
funcionCompara funcion lista condicion = length (filter condicion (map funcion lista))
funcionMenor funcion lista valor = funcionCompara funcion lista (<=valor)
funcionMayor funcion lista valor = funcionCompara funcion lista (>valor)


			   
