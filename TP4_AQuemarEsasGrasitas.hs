--Parte 2 -- TP 3 

--Ejercicio 1 Funcion elProfe

claseProfe ejercicio minutos lpersonas = (map (ejercicio (minutos)).filtrarPersonasNoSaludables) lpersonas

filtrarPersonasNoSaludables lpersonas = filter (not.saludable) lpersonas
--test claseProfe caminata 10 [(10,20,6), (12, 101,2), (20, 20, 8), (24, 40, 1)] = [(12.0,100.66666666666667,2),(24.0,39.947916666666664,1)]
--test2(aplicacionparcial)  claseProfe (pesas 10) [(10,20,6), (12, 101,2), (20, 20, 8), (24, 40, 1)] = [(12,101,2.0),(24,40,1.0)]
-

--Ejercicio 2 Rutina
realizoRutina listaejercicios tiempo persona = foldl (\x y-> y tiempo x) persona listaejercicios 
--test realizoRutina [caminata, pesas 10, caminata] 10 (20,10,2) = (20.0,9.493589743589745,2.0)

--duracionRut tiempo listaejercicios = tiempo `div` tamañoLista (listaejercicios)
--tamañoLista listaejercicios = length listaejercicios
--realizoRutina listaejercicios tiempo persona = foldl (\x y-> y (duracionRut tiempo listaejercicios) x) persona listaejercicios 

--2.1 resumenRutina
resumenRutina nombrerut listaejercicios tiempo persona = (nombrerut, kilosperdidos (realizoRutina listaejercicios tiempo persona) (persona), tonifganada (realizoRutina listaejercicios tiempo persona) (persona))
kilosperdidos  ( _ ,kfinales, _ ) (_,kiniciales,_) = kiniciales-kfinales
tonifganada (_, _ ,tfinal) (_, _ ,tinicial) = tfinal - tinicial 

--test resumenRutina "RutinaFacil" [caminata, entrenamientoEnCinta] 10 (10,60,4) -- ("RutinaFacil",0.3503708854891059,0)

--2.2 muchasRutinas

data Rutina = UnaRutina{lejercicios:: [(Float -> Persona -> Persona)], tiempo::Float}
muchasRutinas listarutinas persona = filter (rutinaSaludable ) listarutinas
rutinaSaludable (UnaRutina lejercicios tiempo) persona = (saludable.realizoRutina lejercicios tiempo) persona
--test muchasRutinas ["Rutina1"[caminata,entrenamientoEnCinta] 10, "Rutina2"[pesas 10, caminata, caminata] 5, "Rutina3"[colina 5, entrenamientoEnCinta] 15] (10,60,4) 

--Ejercicio 3 Diferentes Profesores
data Persona = UnaPersona {edad::Int, peso::Int, tonif::Int} 
data ProfeConEjercicio = UnProfeConEjercicio {ejercicio::(Int -> Persona -> Persona), tiempo::Float}
data ProfeConRutina = UnProfeConRutina {rutina::Rutina, tiempo::Float}
data ProfeVago = UnProfeVago {tiempo::Float}

class Profesor a where
elProfeConData :: a->Persona

instance Profesor ProfeConEjercicio where
elProfeConData (UnProfeConEjercicio ejercicio tiempo) lpersonas = (map (ejercicio (tiempo)).filtrarPersonasNoSaludables) lpersonas

instance Profesor ProfeConRutina where
elProfeConData (UnProfeConRutina rutina tiempo) lpersonas = (map ((realizoRutina rutina tiempo).filtrarPersonasNoSaludables) lpersonas 

instance Profesor ProfeVago where
elProfeConData (UnProfeVago tiempo) lpersonas = (map (filtrarPersonasNoSaludables) lpersonas)
