module TTree where

import Dic

-- Definicion del tipo algebraico TTree k v
data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E  
               deriving Show



-- Ejemplo de un valor t :: TTree Char Int:
t :: TTree Char Integer
t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                           (Node 'o' (Just 2)  (Leaf 'd' 9)
                                                               E 
                                                               (Leaf 's' 4))
                                           E)
                      (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                             (Leaf 'n' 7)
                                                                  E)
                        E)



-- Definamos las siguientes funciones en Haskell para manipular arboles de del tipo
-- TTree k v

-- a) Busca el valor asociado a una clave 
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E             = Nothing
search [] _            = error "Error: invalid key"

search (x:xs) (Leaf k v)        
          | null xs    = if x == k then Just v else Nothing
          | otherwise  = Nothing

search (x:xs) (Node k mv l c r)
          | x == k     = if null xs then mv else search xs c
          | x < k      = search (x:xs) l
          | x > k      = search (x:xs) r



-- b) Agrega un par (clave, valor) a un arbol. Si la clave esta en el arbol, actualiza
-- su valor. Esto indica que no puede haber elementos distintos con claves distintas.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert lx v E             = keyToTTree lx v 

insert lx@(x:xs) nv (Leaf k v) 
               | x == k   = if null xs then Leaf k nv
                                       else Node k (Just v) E (keyToTTree xs nv) E

               | x > k    = Node k (Just v) E E (keyToTTree lx nv)
               | x < k    = Node k (Just v) (keyToTTree lx nv) E E


insert lx@(x:xs) nv (Node k v l c r) 
               | x == k   = if null xs then Node k (Just nv) l c r
                                                            else Node k v l (insert xs nv c) r
                                   
               | x > k    = Node k v l c (insert lx nv r)
               | x < k    = Node k v (insert lx nv l) c r


keyToTTree :: Ord k => [k] -> v -> TTree k v
keyToTTree [x] v = Leaf x v
keyToTTree (x:xs) v = Node x Nothing E (keyToTTree xs v) E


-- c) Elimina una clave y el valor asociado a esta en un arbol
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

-- d) Devuelve una lista ordenada con las claves del mismo
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k _) = [[k]]
keys (Node k v l c r) = keys l ++ (ifKey k v) ++ map (k:) (keys c) ++ keys r 

     where 
          ifKey :: k -> Maybe v -> [[k]]
          ifKey _ Nothing  = []
          ifKey k (Just _) = [[k]]



-- Ahora demos una instancia de la clase Dic para el tipo de datos TTree k v
instance Ord k => Dic [k] v (TTree k v) where

     vacio    = E
     insertar = insert
     buscar   = search
     eliminar = delete
     claves   = keys

 
t1 = insert "re" 16 E
t2 = insert "sin" 7 t1
t3 = insert "si" 4 t2
t4 = insert "se" 8 t3
t5 = insert "reo" 2 t4
t6 = insert "red" 9 t5
t7 = insert "res" 4 t6
t8 = insert "ras" 1 t7