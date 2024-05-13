--Ejercicio 1
--Defino el tipo de los naturales
data Nat = Zero | Succ Nat

--Ejerciio 2
--Funcion que traduce un natural a un entero
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n --n puede ser otro succ(succ nat) por ej, por eso no se pone directamente succ nat

--Ejercicio 3
--Funcion que traduce un entero a un natural
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))

--Ejercicio 4
--Funcion que suma dos numeros naturales
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat n Zero = n
sumaNat (Succ n) m = Succ (sumaNat n m) -- uso la idea de como escribir los naturales matematicamente, ej 2 se escribe como 1+1 
--entonces se escribe como succ (sumaNat n m) donde sumaNat n seria la descomposicion de succ n y el m queda asi porque no importa
--cómo está escrito el segundo parametro por ésta razón : 2 + 3 == 1+ (1+3) el primer 1 sería el succ
--el segundo 1 es el sumaNat n y el 3 es el m

--Ejercicio 5
--Instanciacion de Eq, Ord y show para los nat
instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    Zero == Zero = True
    Succ n == Succ m = n == m
    _ == _ = False

instance Ord Nat where
    (<=) :: Nat -> Nat -> Bool
    Zero <= _ = True
    Succ n <= Zero = False
    Succ n <= Succ m = n <= m

instance Show Nat where 
    show :: Nat -> String
    show Zero = "Zero"
    show (Succ n) = "Succ (" ++ show n ++ ")"

--Ejercicio 6
--defino los arboles binarios
data Arbol a = Nil | Node (Arbol a) a (Arbol a)

--Ejercicio 7
--funcion que retorna el numero de nodos del árbol
size :: Arbol a -> Int
size Nil = 0
size (Node hz a hd)  = 1 + size hz + size hd

--Ejercicio 8
--funcion que retorna la altura del arbol
height :: Arbol a -> Int
height Nil = 0
height (Node hz a hd) = 1 + max (height hz) (height hd)

--Ejercicio 9
--instanciacion de eq, ord y show

instance Eq (Arbol a) where
    (==) :: Arbol a -> Arbol a -> Bool
    Nil == Nil = True
    Node hi1 a1 hd1  == Node hi2 a2 hd2 = hi1 == hi2 && hd1 == hd2 
    _ == _ = False

instance Ord (Arbol a) where
    (<=) :: Arbol a -> Arbol a -> Bool
    Nil <= _ = True
    Node hi1 a1 hd1 <= Node hi2 a2 hd2 = hi1 <= hi2 && hd1 <= hd2

{-instance Show (Arbol a) where
    show :: Arbol a -> String
    show Nil = "Nil"
    show (Node hi a hd) = "Node (" ++ show hi ++ ") " ++ (show a) ++ " (" ++ show hd ++ ")"-}