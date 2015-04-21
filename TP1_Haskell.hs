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

punto 5)
filtradoRadio lista = filter (<=5) lista
filtradoNoRadio lista = not (filtradoradio lista)
tamañoRadio lista = length (filtradoradio lista) 
tamañonoRadio lista = length (filtradonoradio lista)
centrada lista = (tamañonoradio lista) > (tamañonoradio lista)
