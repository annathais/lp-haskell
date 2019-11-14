 module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

import Tipos

play Tabuleiro = do
  print Tabuleiro



  putStrLn "Escolha uma coordenada (x,y): "
  newTabuleiro <- parseInput Tabuleiro <$> getLine  -- atualizar Tabuleiro



  if jogoTerminou newTabuleiro then                 -- definir "jogoTerminou" (quando o jogo termina? se resultado != jogando)
    return $ Resultado newTabuleiro                 -- setar resultado quando jogoTerminou -> Vitoria ou derrota?
  else play Tabuleiro                               -- recursão