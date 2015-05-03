--Ejercicio 1) esSaludable

saludable persona =   esFlaco persona && personaTonificada persona 

esFlaco (_,peso,_) = peso<100 

personaTonificada (_,_,tonificación) = tonificación > 5

-- Ejercicio 2) quemarCalorias

quemarCalorias (edad, peso, tonif) calorias | not (esFlaco (edad, peso, tonif)) = (edad, peso-calorias/150, tonif)
											| esFlaco (edad, peso, tonif) && edad>30 && calorias>200 = (edad, peso-1, tonif)										
											| otherwise = (edad, peso-(calorias/(peso*edad)),tonif)

-- Ejercicio 3) caminata, entrenamientoEnCinta, pesas, colina y montaña

caminata minutos persona = quemarCalorias persona (minutos*5*1) 

entrenamientoEnCinta minutos persona = quemarCalorias persona (((6+(minutos/5))/2)*40)

pesas kiloslevantados minutos  (edad,peso,tonif)| minutos>10 = (edad,peso, (tonif)+(kiloslevantados/10))
												| otherwise = (edad, peso, tonif)

colina inclinacion minutos persona = quemarCalorias persona (inclinacion*minutos*2)

montana inclinacion minutos (edad, peso, tonif) = (colina (inclinacion+3) (minutos/2) . colina inclinacion (minutos/2)) (edad, peso, tonif+1)
