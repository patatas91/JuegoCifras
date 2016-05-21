-- Cristian Simon Moreno
-- Nip: 611487

import Data.List

data Operacion = Suma | Resta | Mult | Div
          deriving Eq
		  
data Expresion = Num Int | Apl Operacion Expresion Expresion
		  deriving Eq
 
instance Show Operacion where
   show Suma = "+"
   show Resta = "-"
   show Mult = "*"
   show Div = "/"
   
instance Show Expresion where
   show (Num n)     = show n
   show (Apl op x y) = parentesis x ++ show op ++ parentesis y
                      where
                         parentesis (Num n) = show n
                         parentesis e       = "(" ++ show e ++ ")"
						 
oper :: [Operacion]
oper = [Suma,Resta,Mult,Div]

-- Comprueba que el resultado de una operacion es un natural   
opCorrecta :: Operacion -> Int -> Int -> Bool
opCorrecta Suma _ _ = True
opCorrecta Mult _ _ = True
opCorrecta Resta x y = x > y
opCorrecta Div x y = x `mod` y == 0

-- Dada una operacion y dos numeros realiza dicha operacion
operar :: Operacion -> Int -> Int -> Int
operar Suma x y = x + y
operar Resta x y = x - y
operar Mult x y = x * y
operar Div x y = x `div` y

-- Extrae los numeros de una expresion
obtenerNum :: Expresion -> [Int]
obtenerNum (Num n)     = [n]
obtenerNum (Apl _ x y) = obtenerNum x ++ obtenerNum y

-- Calcula el valor de una expresion
valor :: Expresion -> [Int]
valor (Num n)     = [n | n > 0]
valor (Apl op a b) = [operar op x y | x <- valor a
                                  , y <- valor b
                                  , opCorrecta op x y]

-- Devuelve una lista de sublistas
sublistas :: [a] -> [[a]]
sublistas = subsequences

-- Devuelve una lista de listas con un valor intercalado para todos los elementos
intercalar :: a -> [a] -> [[a]]
intercalar x []     = [[x]]
intercalar x (y:ys) = (x:y:ys) : map (y:) (intercalar x ys)

-- Devuelve una lista con todas las permutaciones
permutar :: [a] -> [[a]]
permutar = permutations

-- Devuelve una lista con todas las posibles combinaciones
combinaciones :: [a] -> [[a]]
combinaciones xs = concat (map permutar (sublistas xs))

-- Separa una lista en todas sus posibles separaciones
separar :: [a] -> [([a],[a])]
separar []     = []
separar [_]    = []
separar (x:xs) = ([x],xs) : [(x:is,ds) | (is,ds) <- separar xs]

-- Combina dos expresiones con las operaciones disponibles
combinar :: Expresion -> Expresion -> [Expresion]
combinar a b = [Apl op a b | op <- oper]

-- Dada una lista devuelve todas las posibles expresiones que se pueden formar
expresiones :: [Int] -> [Expresion]
expresiones []  = []
expresiones [n] = [Num n]
expresiones ns  = [e | (is,ds) <- separar ns
                     , i       <- expresiones is
                     , d       <- expresiones ds
                     , e       <- combinar i d]					 

-- Devuelve todas las soluciones
soluciones :: [Int] -> Int -> [Expresion]
soluciones ns n =  [e | ns' <- combinaciones ns
                      , e   <- expresiones ns'
                      , valor e == [n]
					  , length (obtenerNum e) == length ns]

-- Comprueba si dada una lista y un valor se puede, operando, alcanzar dicho valor. Si se
-- puede muestra las soluciones					  
jugar :: [Int] -> Int -> [Expresion]
jugar lista n 
	| length(soluciones lista n) > 0 = soluciones lista n
	| otherwise = error "-> No es posible encontrar una expresion para esos valores\n"
	
	
----------------------------

main = do
	putStrLn ""
	putStrLn "----------------------"
	putStrLn "     JUEGO CIFRAS     "
	putStrLn "Cristian Simon Moreno "
	putStrLn "     NIP: 611487      "
	putStrLn "----------------------"
	putStrLn ""
	putStr "-> Introduzca una lista de numeros naturales: "
	l <- readLn
	putStr "-> Introduzca el numero para el que desea comprobar: "
	n <- readLn
	putStrLn ""
	putStrLn "----------------------"
	putStr "Lista: "
	print l	
	putStr "Evaluacion para: "
	print n
	putStrLn "----------------------"
	putStrLn ""
	putStrLn "-> RESULTADO: "
	print (jugar l n)
	putStr "-> TOTAL: "
	print (length(jugar l n))
	putStrLn ""
	
	
	