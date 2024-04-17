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

insert l v E = linea l v 

insert [x] nv (Leaf k v) | x == k  = Leaf k nv
                         | x > k   = Node k (Just v) E E (Leaf x nv) 
                         | x < k   = Node k (Just v) (Leaf x nv) E E

insert (x:xs) nv (Leaf k v) | x == k = Node k (Just v) E (linea xs nv) E
                            | x > k  = Node k (Just v) E E (linea xs nv)
                            | x < k  = Node k (Just v) (linea xs nv) E E








insert [x] nv (Node k v l c r) | x == k = Node k (Just nv) l c r
                               | x > k  = Node k v l c (insert [x] nv r)
                               | x < k  = Node k v (insert [x] nv l) c r

insert (x:xs) nv (Node k v l c r) | x == k   = Node k v l (insert xs nv c) r
                                  | x > k    = Node k v l c (insert (x:xs) nv r)
                                  | x < k    = Node k v (insert (x:xs) nv l) c r


linea :: Ord k => [k] -> v -> TTree k v
linea [x] v = Leaf x v
linea (x:xs) v = Node x Nothing E (linea xs v) E


-- c) Elimina una clave y el valor asociado a esta en un arbol
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

-- d) Devuelve una lista ordenada con las claves del mismo
-- TODO: Optimizar con funcion auxiliar que tenga como argumento el prefijo que estamos
-- TODO  llevando en esa parte del arbol
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
