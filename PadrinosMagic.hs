--RECURSIVIDAD http://stackoverflow.com/questions/14820138/understanding-recursion-in-haskell
--https://docs.google.com/document/d/1BjlypGu9abV9bY_qMt0OiSSBuSZ7wisZfxwGINgfxiQ/edit
--CLASE PDEP https://docs.google.com/document/d/1le1Pxc2z_9f35U8JEqRwR_AxI3phcOp7IAaR1X00zWs/pub
--CLASE PDEP https://docs.google.com/document/d/1Ydc1qZNOcCMP5PQlP1zjB75cRXILQ5cbeqmFslOPxIA/edit
--FAMILIA FOLD https://docs.google.com/document/d/1jSrU7lVMan4nbHBETGqvO5VpqJI0KXVWtH7fqnVASPU/edit#


data Chico = Unchico {nombre :: String, edad :: Int, habilidades :: [String], deseos :: [Chico->Chico]}  
instance Show Chico where
  show c = "Soy el chico " ++ (nombre c) ++ "y tengo " ++ show (edad c) ++ " años " ++ "y tengo las habilidades " ++ show (habilidades c)

aprenderHabilidades  habilidadnueva (Unchico nombre edad habilidades deseos )  =  Unchico nombre edad (habilidadnueva ++ habilidades) deseos
 
--let nicasio = Unchico { nombre = "Nico",  edad = 20, habilidades = ["correr", "jugar"]}
--Main> aprendeHabilidades nicasio [ "play", "saltar"]
--["play","saltar","correr","jugar"]

-- ¿Porque como lo defini yo antes no va a la hora de definir un chiconuevo, serMayor (Unchico nombre edad habilidades deseos) nuevaedad = Unchico nombre nuevaedad habilidades)

serMayor unChico = cambioEdad unChico 18
cambioEdad (Unchico n _ h d) edadNueva = Unchico n edadNueva h d

--2) CUMPLIR DESEOS

wanda (Unchico n e h d) = (madurar.cumplirPrimerDeseo) (Unchico n e h d)
cosmo (Unchico n e h d) = cambioEdad (Unchico n e h d)(div e 2)

cosmo2 unchico = cambioEdad unchico (div (edad unchico) 2)
madurar (Unchico n e h d) = cambioEdad (Unchico n e h d) (e+1)  
cumplirPrimerDeseo (Unchico nom edad habilidades deseos) = (deseos !!0) (Unchico nom edad habilidades deseos) 

muffinMagico unchico = (\x-> )
--let nicasio = Unchico { nombre = "Nico",  edad = 20, habilidades = ["correr", "jugar"] [serMayor, aprenderHabilidades ["saltar", "matar"]]}
--muffinMagico nicasio = 
