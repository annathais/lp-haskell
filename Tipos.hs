module Tipos where

data Estado a = Bomba | Fechado | Peso a deriving(Show)

type Coordenada    = (Int, Int)
type Minas    = [[Estado Int]]
type MapaPeso  = [[Int]]
type Campo = [[Estado Int]]
