
data Persona = UnaPersona {edad::Int, peso::Int, tonif::Int} 
data Rutina = UnaRutina( Float, [(Float -> Persona -> Persona)])

--Parte 2
--1 Funcion elProfe
claseProfe ejercicio minutos lpersonas = (map (ejercicio (minutos)).filtrarPersonasNoSaludables) lpersonas

filtrarPersonasNoSaludables lpersonas = filter (not.saludable) lpersonas

--2 Rutina

--¿De esta forma? realizoRutina listaejercicios tiempo persona = foldl (\x y-> y (duracionRut tiempo listaejercicios) x) persona listaejercicios 
duracionRut tiempo listaejercicios = tiempo `div` tamañoLista (listaejercicios)
tamañoLista listaejercicios = length listaejercicios

realizoRutina listaejercicios tiempo persona = foldl (\x y-> y (duracionRut tiempo listaejercicios) x) persona listaejercicios 

