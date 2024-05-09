
--1) Generar una lista infinita de unos.

inf :: [Int]
inf =  1 :  inf    


--2). Generar una lista infinita de naturales comenzando desde un n ́umero dado.
infNat :: Int -> [Int]
infNat n = n : infNat(n+1)

--3) Generar una lista con los primeros n naturales

pNat :: Int -> [Int]
pNat n 
        | n == 0 = []
        | otherwise = n : pNat(n-1)

 --4)  Retornar los primeros 5 elementos de una lista infinita de enteros positivos.

intPos :: [Int] -> [Int]
intPos  xs  = take 5 xs
--Todo en uno usando el enum
intPos' :: Int->[Int] 
intPos' n = take n[1..]


--Generador de lista inf
numInf :: [Int]
numInf = enumFrom 0

--Utilizando funciones de alto orden resolver:

--5) Dada una lista de enteros, retornar sus cuadrados, es decir, dado [x0, x1, . . . , xn]

retQ :: [Int]->[Int] 
retQ []=[]
retQ (x:xs) = (x*x):retQ(xs)

mapCuadrado :: (Num a) => [a] -> [a]
mapCuadrado [] = error "Empty List"
mapCuadrado list = map (\x -> x*x) list

-- Ejemplo de uso
-- mapCuadrado [1, 2, 3, 4, 5] devuelve [1, 4, 9, 16, 25]
--6) dado un entero positivo, retornar una lista de sus divisores 
retDiv :: Int->[Int]
retDiv n = [x | x <- [1..n], n `mod` x == 0 ]   

divisores :: Int -> [Int]
divisores n = filter (\x ->  mod n x == 0) [1..n]

--7. Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros
--primos de la lista original.

--primNum :: [Int]->[Int]
--primNum [a] = filter (\x -> x `mod` x == 0)


--8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.
retSum :: [Int]->Int
retSum [] = error "empty list"
retSum xs = foldr (\x acc-> acc + x*x) 0 xs 


--9. Dada una lista de naturales, retornar la lista con sus sucesores.
retSucc :: [Int]->[Int] 
retSucc xs = map (+1) xs 

--10. Dada una lista de enteros, sumar todos sus elementos.

retSum1 :: [Int]->Int 
retSum1 (x:xs) = foldl (\x acc-> acc + x) 0 (x:xs)

-- 11 Definir el factorial usando fold.
factFold :: Int->Int
factFold x = foldl (*) 1 [1..x]


--12 *. Redefinir la funci ́on 'and' tal que 'and xs' se verifica si todos los elementos de xs son verdaderos. 

--Por ejemplo: and [1<2, 2<3, 1/=0] = True,
--and [1<2, 2<3, 1 == 0] = False.

andxs :: [Bool]-> Bool 
andxs [] = True
andxs (x:xs) = x && andxs xs  

andxs' :: [Bool] -> Bool 
andxs' [] = True 
andxs' xs = foldl (&&) True xs



--13. Usando foldl o foldr definir una funci ́on tam::[a]->Int que devuelve la
--cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
--foldl evaluen diferente con los mismos parametros.

tam :: [a] -> Int 
tam [] = 0 
tam (x:xs) = foldl (\acc _ -> acc + 1) 1 xs 



--Utilizando listas por comprensi ́on resolver:

--14. Dada una lista de enteros, retornar sus sucesores.

retSucc' :: [Int]->[Int]
retSucc' xs = [x + 1 | x <- xs] 

--15 *. Dada una lista de naturales, retornar sus cuadrados.
retCuad :: [Int]->[Int]
retCuad xs = [x*x | x<-xs]

--16. Dada una lista de enteros, retornar los elementos pares que sean mayores a 10.
retPar :: [Int]->[Int]
retPar xs = [x | x<-xs,even x, x>10]



--17. Dado un entero, retornar sus divisores.

retDiv' :: Int->[Int] 
retDiv' n = [x | x<-[1..n], n `mod` x == 0]

--18 *. Definir la funci ́on todosOcurrenEn :: Eq a => [a] -> [a] -> Bool tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son ele-
--mentos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True, todosOcurrenEn [1,5,2,5] [5,2,4] = False

sameElems :: Eq a => [a] -> [a] -> Bool 
sameElems xs ys = and [elem x ys | x<-xs]

sameElems' :: Eq a => [a] -> [a] -> Bool 
sameElems' xs ys = and [x `elem` ys | x<-xs]

--19. Dado un natural n, retornar los n umeros primos comprendidos entre 2 y n.
retPrim :: Int -> [Int]
retPrim n = [x | x<-[2..n],esPrim x]

esPrim :: Int -> Bool 
esPrim n = n>1 && (length[x | x<-[2..n-1],n `mod` x == 0])==0

--20. Dadas dos listas de naturales, retornar su producto cartesiano.
retCart :: [Int]->[Int]->[(Int,Int)]
retCart xs ys = [(x,y) | x<-xs,y<-ys]

retCart1 :: [a]->[b]->[(a,b)]
retCart1 xs ys = [(x,y) | x<-xs , y<-ys]
--21 *. Dadas una lista y un elemento retornar el numero de ocurrencias del elemento x en la lista ys.
retOcur :: Eq a => [a]->a->Int 
retOcur xs n = length [x | x<-xs, x == n]

--22. Escribir la funci ́on split2 :: [a] - > [([a],[a])], que dada una lista xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
--split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].

split2 :: [a]->[([a],[a])]
split2 [] = [([],[])]
split2 xs = [splitAt i xs | i<-[0..length xs]]
--23 *. Definir una funci ́on que, dada una lista de enteros, devuelva la suma de la suma de todos los segmentos iniciales.
--Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.

sumaSeg :: [Int] -> Int
sumaSeg xs = sum [sum (take i xs) | i <- [0..length xs]]




--24. Definir la lista infinita de los numeros pares.

--[x | x<-[0,2..]]

infPares :: IO()
infPares = mapM_ print [x | x<-[0,2..]] 

infParesHasta :: Int -> [Int]
infParesHasta n = [x | x<-[0,2..n]]


