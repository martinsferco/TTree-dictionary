module TTree where
import Dic

-- Definicion del tipo algebraico TTree k v
data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E  
               deriving (Eq, Show)


{-
--------------------- CONSIDERACIONES ---------------------

> Consideramos que los valores de la forma (Node k (Just v) E E E) deben ser reemplazados
por (Leaf k v) y los valores (Node k Nothing E E E) deben ser directamente E.

> Ademas, tampoco son validos los nodos de la forma (Node k Nothing l E r), pues un nodo sin
valor asociado y sin un hijo del medio puede ser reemplazado por otro nodo.

> La interfaz se encarga de que este tipo de nodos no esten presentes en el arbol; si 
se realizan operaciones directamente con los datos algebraicos pueden producirse errores.

> No aceptamos claves vacias en funciones. En esos casos, se generara un error.

---------------------------------------------------------
-}


-- Devuelve un TTree k v vacio.
empty :: Ord k => TTree k v
empty = E


-- a) Busca el valor asociado a una clave.
search :: Ord k => [k] -> TTree k v -> Maybe v
search [] _              = error "Error: keys must be non-empty"
search _ E               = Nothing

search (x:xs) (Leaf k v) = if x == k && null xs then Just v 
                                                else Nothing

search lx@(x:xs) (Node k mv l c r)
          | x == k       = if null xs then mv else search xs c
          | x < k        = search lx l
          | x > k        = search lx r



-- b) Agrega un par (clave, valor) a un arbol. Si la clave esta en el arbol, actualiza
-- su valor. Esto indica que no puede haber claves iguales con valores diferentes.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ _        = error "Error: keys must be non-empty"
insert lx v E        = insertIntoEmpty lx v 

insert lx@(x:xs) nv (Leaf k v) 
          | x == k   = if null xs then Leaf k nv
                                  else Node k (Just v) E (insertIntoEmpty xs nv) E

          | x > k    = Node k (Just v) E E (insertIntoEmpty lx nv)
          | x < k    = Node k (Just v) (insertIntoEmpty lx nv) E E

insert lx@(x:xs) nv (Node k v l c r) 
          | x == k   = if null xs then Node k (Just nv) l c r
                                  else Node k v l (insert xs nv c) r
                                   
          | x > k    = Node k v l c (insert lx nv r)
          | x < k    = Node k v (insert lx nv l) c r


-- Dada una llave [k] y un valor v, insertIntoEmpty retorna el arbol que se obtiene de insertar
-- este par clave-valor en un arbol vacio, que es un arbol solo con hijos del medio donde el unico
-- que lleva un valor es el ultimo (una hoja).
insertIntoEmpty :: Ord k => [k] -> v -> TTree k v
insertIntoEmpty [x] v      = Leaf x v
insertIntoEmpty (x:xs) v   = Node x Nothing E (insertIntoEmpty xs v) E


-- c) Elimina una clave y el valor asociado a esta en un arbol.

{-
     Observacion. Tambien puede implementarse delete sin hacer llamados a replace.
     El arbol resultante seguira siendo valido, pero ocasionalmente dara lugar a
     nodos de la forma

                                        N k E l E r

     que no son necesarios, pues al no tener un valor asociado ni ser parte del
     prefijo de alguna clave, el nodo puede ser reemplazado. De esto se encarga
     la funcion replace poniendo en su lugar, analogamente al caso de los BST,
     el nodo mas a la izquierda del subarbol r (con ciertas modificaciones).
-}

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete [] _                 = error "Error: keys must be non-empty"
delete _ E                  = E

delete (x:xs) lf@(Leaf k v) = if x == k && null xs then E
                                                   else lf

delete lx@(x:xs) (Node k v l c r) 
          | x == k          = if null xs then replace (Node k Nothing l c r)
                                         else replace (convert (Node k v l (delete xs c) r))
          | x > k           = convert (Node k v l c (delete lx r))
          | x < k           = convert (Node k v (delete lx l) c r)

          where

               replace :: Ord k => TTree k v -> TTree k v
               replace (Node _ _ l E E) = l
               replace (Node _ _ E E r) = r 
               replace (Node _ _ l E r) = let (Node k v _ c _, r') = minAux r 
                                          in   Node k v l c r' 
               replace t                = t


-- Convierte a los nodos que tienen tres subarboles vacios en un nodo valido.
convert :: Ord k => TTree k v -> TTree k v
convert (Node k (Just v) E E E) = Leaf k v
convert (Node k Nothing E E E)  = E 
convert t                       = t


-- Dado un TTree k v, minAux devuelve un par de TTree k v tal que
--   El primer arbol, es el nodo mas a la izquierd del arbol, con su correspondiente
--  subarbol central (si es que lo tiene).
--   El segundo arbol, es el arbol original pero habiendo eliminado el nodo devuelto
--  en la primera componente de la tupla.

minAux :: Ord k => TTree k v -> (TTree k v, TTree k v)
minAux E                 = error "Error: cannot determine minimum of an empty tree"
minAux (Leaf k v)        = (Node k (Just v) E E E, E)
minAux (Node k v E c r)  = (Node k v E c E, r)
minAux (Node k v l c r)  = let (minNode, l') = minAux l
                           in  (minNode, convert (Node k v l' c r))


-- d) Devuelve una lista ordenada con las claves del mismo
keys :: TTree k v -> [[k]]
keys E                = []
keys (Leaf k _)       = [[k]]
keys (Node k v l c r) = keys l ++ ifKey k v ++ map (k:) (keys c) ++ keys r 

     where 
          ifKey :: k -> Maybe v -> [[k]]
          ifKey _ Nothing  = []
          ifKey k (Just _) = [[k]]

-- Ahora demos una instancia de la clase Dic para el tipo de datos TTree k v
instance Ord k => Dic [k] v (TTree k v) where

     vacio    = empty
     insertar = insert
     buscar   = search
     eliminar = delete
     claves   = keys

 

-- Ejemplo de TTree Char Integer proporcionado en el TP -----------------------
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

-- Construimos el ejemplo de TTree Char Integer proporcionado en el tp
--  a partir de nuestra funcion insertar de la clase Dic.

t1 :: TTree Char Int
t1 = insertar "re" 16 vacio
t2 = insertar "sin" 7 t1
t3 = insertar "si" 4 t2
t4 = insertar "se" 8 t3
t5 = insertar "reo" 2 t4
t6 = insertar "red" 9 t5
t7 = insertar "res" 4 t6
t8 = insertar "ras" 1 t7

