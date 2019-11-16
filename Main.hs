module Main where

import System.Random

vazio = 0
vazioSinalizado = 1
vazioLimpo = 2
bomba = 3
bombaSinalizada = 4

tamanhoTab = 20

main :: IO ()
main = do
  putStrLn "Bem vindo ao Campo Minado"
  gerar <- getStdGen
  let tabuleiro = gerarTabuleiro tamanhoTab tamanhoTab gerar
  jogo tabuleiro tamanhoTab


jogo tabuleiro tamanhoTab = do
  imprimirTab tabuleiro False
  bloco <- dadoEntrada tamanhoTab
  let encontrouBomba = encontrouBombaVerify tabuleiro bloco
  if encontrouBomba then do
    imprimirTab tabuleiro True
    putStrLn "Você Perdeu!!!"
  else do
    let novoTab = atualizaTab tabuleiro bloco
    if vitoriaJogoVerify novoTab then do
      imprimirTab novoTab False
      putStrLn "Você Ganhou!!!"
    else do
      jogo novoTab tamanhoTab
  
gerarTabuleiro :: RandomGen g => Int -> Int -> g -> [[Int]]
gerarTabuleiro largura altura gerar = make2DList largura altura (numRandomicos largura altura gerar)
  where
    numRandomicos largura altura gerar = map (\x -> if x == 1 then bomba else vazio) (take (largura * altura) (radomicoRs (0::Int, 4::Int) gerar))
    make2DList largura altura lista = [[lista!!((altura * i) + j) | j <- [0..largura-1]] | i <- [0..altura-1]]
  
dadoEntrada tamanhoTab = do
  putStrLn "Escolha uma coordenada (x,y): "
  input <- getLine
  let bloco = analiseEntrada input
  let valido = entradaValidada tamanhoTab bloco
  if valido then
    return bloco
  else do
    putStrLn "Coordenada Errada"
    dadoEntrada tamanhoTab









-- import Tipos

-- play Tabuleiro = do
--   print Tabuleiro



--   putStrLn "Escolha uma coordenada (x,y): "
--   newTabuleiro <- parseInput Tabuleiro <$> getLine  -- atualizar Tabuleiro



--   if jogoTerminou newTabuleiro then                 -- definir "jogoTerminou" (quando o jogo termina? se resultado != jogando)
--     return $ Resultado newTabuleiro                 -- setar resultado quando jogoTerminou -> Vitoria ou derrota?
--   else play Tabuleiro                               -- recursão