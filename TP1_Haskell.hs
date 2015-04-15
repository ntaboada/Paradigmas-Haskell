
Punto 3)

lengthStrings lista_cadenas = map (length) lista_cadenas 

filterSmallStrings lista_lengths = filter (<=3) lista_lengths

filterBigStrings lista_lengths = filter (>3) lista_lengths

mensajito lista_cadenas | length (filterSmallStrings (lengthStrings lista_cadenas)) > length (filterBigStrings (lengthStrings lista_cadenas))  = True
						            |  length (filterSmallStrings (lengthStrings lista_cadenas)) < length (filterBigStrings (lengthStrings lista_cadenas))  = False
						
mensajitoOK lista_cadenas | length (filterSmallStrings (lengthStrings lista_cadenas)) > length (filterBigStrings (lengthStrings lista_cadenas))  = "Es mensajito"
						              |  length (filterSmallStrings (lengthStrings lista_cadenas)) < length (filterBigStrings (lengthStrings lista_cadenas))  = "No es mensajito"
						              | otherwise = "Misma cantidad"

Punto 4) 

Empezando a hacer el Punto 4
