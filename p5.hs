--Ejercicio 1
--lista infinita de 1
inf1 :: [Int]
inf1 = 1 : inf1

--Ejercicio 2
--Lista infinita a partir de un n dado
infn :: Int ->[Int]
infn n = n : infn (n+1)

--Ejercicio 3
--Funcion que genera los primeros n naturales

infNat :: Int -> [Int]
infNat 0 = [0] 
infNat n = infNat (n-1) ++ [n]

--Ejercicio 4
--Funcion que retorna los primeros 5 elementos de una lista inf de enteros positivos
numeros :: [Int]
numeros = enumFrom 0

inf5 :: [Int] -> [Int]
inf5 numeros = take 5 numeros

--Ejercicio 5
--Dada una lista de enteros, retornar sus cuadrados
cuad :: [Int] -> [Int]
cuad [] = []
cuad (x:xs) = map (^2) (x:xs) 

--Ejercicio 6
--Dado un entero, retornar sus divisores

esDiv :: Int -> Int-> Bool
esDiv n 0 = False
esDiv 0 n = False
esDiv n x= mod n x == 0 

divisores :: Int -> [Int]
divisores n = filter (esDiv n) [-n..n]

--Ejercicio 7
--dada una lista de naturales, obtener una lista de los naturales que son primos
primos :: Int -> Int -> Bool
primos x 1 = False
primos x y = mod x y == 0 || primos x (y-1)

prim2 :: Int -> Bool
prim2 1 = False 
prim2 2 = True
prim2 x = not (primos x (x-1))

primm :: [Int] -> [Int]
primm xs = filter prim2 xs

--Ejercicio 8 
--Dada una lista de naturales, devolver la suma de los duadrados de la lista
sumCuad :: [Int]-> Int
sumCuad xs = sum (map (^2) xs)

--Ejercicio 9
--dada una lista de nat, retornar la lista de los sucesores
succ :: [Int] -> [Int]
succ [] = error "Lista Vacia"
succ (x:xs) = map (+1) (x:xs)

--Ejercicio 10
--Dada una lista de enteros, sumar sus elementos
suma :: [Int] -> Int
suma [] = error "Lista vacia, no hay elementos para sumar"
suma (x:xs) = foldl (+) 0 (x:xs)

--Ejercicio 11
--Hacer el factorial usando fold
factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

--Ejercicio 12
--redefinir el and usando fold
andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

--Ejercicio 13
--funcion que devuelve la cantidad de elementos de una lista
tam :: [a] -> Int
tam [] = error "lista vacia"
tam (x:xs) = foldl s 0 (x:xs)

s :: Int -> a -> Int
s x a = x+1

--Ejercicio 14
listaSuc :: [Int] -> [Int]
listaSuc xs = [x+1 | x <- xs]

--Ejercicio 15
listaCuad :: [Int] -> [Int]
listaCuad xs = [ x * x | x <- xs]

--Ejercicio 16
listaPar :: [Int] -> [Int]
listaPar xs = [ x | x <- xs , even x, x > 10]

{--Ejercicio 17
listaDiv :: [Int] -> [Int]
listaDiv xs -}

--Ejercicio 19
numPrim :: Int -> [Int]
numPrim n = [z | z <- [2..n], esPrimo z ] 
esPrimo :: Int-> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo n = null [x | x <- [2..n-1], mod n x == 0]

--Ejercicio 20
cartesiano :: [a] -> [b] -> [(a,b)]
cartesiano xs ys = [(x,y) | x <- xs, y <- ys]

--Ejercicio 21
--dada una lista y un elemento retornar el numero de ocurrencias del elemento x en ys
numberOcurrences :: Eq a => a -> [a] -> Int
numberOcurrences x ys = length [z| z <- ys , z == x]

--Ejercicio 22
split2 :: [a] -> [([a],[a])]
split2 xs = [(take a xs, drop a xs) | a <- [0..length xs]]

--Ejercicio 23 (con el take obtengo los subsegmentnos iniciales de una lista)
--dada una lista de enteros, devolver la suma de todos los subsegmentos
sumaSegment :: [Int] -> Int
sumaSegment xs = sum [sum (take a xs) | a <- [0..length xs]]

--Ejercicio 24
--lista infinita de numeros pares
infiPar :: [Int] 
infiPar = [x | x <- [0..], even x]

