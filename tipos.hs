module Tipos where

import Data.Matrix

type Coordenada = (Int, Int)

type Status = Bool                                            -- 0: Fechado; 1: Aberto

type Quadrado = (Status, Bool, Int) deriving (Eq)             -- Status; Bool: isBomb?; Int: qtd de bombas vizinhas

type Tabuleiro = Matrix Quadrado                              -- Tabuleiro = matriz de itens "Quadrado"

type Resultado = Vitoria | Derrota | Jogando deriving (Show)  -- Resultado para manter no loop da main ou sair
