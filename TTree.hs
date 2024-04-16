module TTree where

import Dic

-- Definicion del tipo algebraico TTree k v
data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E  




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
search = undefined

-- b) Agrega un par (clave, valor) a un arbo. Si la clave esta en el arbol, actualiza
-- su valor. Esto indica que no puede haber elementos distintos con claves distintas.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert = undefined

-- c) Elimina una clave y el valor asociado a esta en un arbol
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

-- d) Devuelve una lista ordenada con las claves del mismo
-- TODO: Optimizar con funcion auxiliar que tenga como argumento el prefijo que estamos
-- TODO  llevando en esa parte del arbol
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k _) = [[k]]
keys (Node k Nothing l c r) = keys l ++ map (k:) (keys c) ++ keys r 
keys (Node k (Just v) l c r) = keys l ++ [[k]] ++ map (k:) (keys c) ++ keys r 


-- Ahora demos una instancia de la clase Dic para el tipo de datos TTree k v
instance Ord k => Dic [k] v (TTree k v) where

     vacio    = E
     insertar = insert
     buscar   = search
     eliminar = delete
     claves   = keys
