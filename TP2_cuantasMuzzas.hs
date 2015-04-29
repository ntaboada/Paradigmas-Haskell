pidioDe sabor (x,_)   = x==sabor

porcionesDeSabor sabor lista  = filter (pidioDe sabor) lista

cuantasPorcionesSabor sabor lista = (sum.map snd.porcionesDeSabor sabor) lista

cuantasPizzas sabor lista |  (mod (cuantasPorcionesSabor sabor lista) 8 == 0) = div (cuantasPorcionesSabor sabor lista)  8 
			  | otherwise = (div (cuantasPorcionesSabor sabor lista)  8 )+1
			  
			  Ejemplo: cuantasPizzas "muzza" [("muzza", 2), ("muzza", 3), ("faina", 1), ("muzza", 5), (" napo", 5), ("napo", 9)]
			  
			  Devuelve---> 2 
