{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  FunctionalDependencies #-}


module Dic where

-- Definimos la clase Dic
class Dic k v d | d -> k v where
  
  vacio    :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar   :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves   :: Ord k => d -> [k]  

