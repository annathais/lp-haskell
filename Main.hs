module Main where

import Control.Monad
import System.Random    -- Esta biblioteca lida com a tarefa comum de geração de números pseudo-aleatórios
import Data.List        -- Esta biblioteca permite dazer operações em listas.
import System.IO        -- Esta biblioteca padrão de IO.
import Tipos            -- Módulo onde está definido os tipos utilizados


espaco   = 3
largura  = 20
altura = 20

main :: IO ()
main = do
  putStrLn "\n--------------------------------------------------------------"
  putStrLn "--            --> Bem vindo ao Campo Minado <--             --"
  putStrLn "--------------------------------------------------------------\n"
  g        <- getStdGen                                   -- Obtém o gerador global de números aleatórios.
  campo    <- return $ fazMatriz largura altura Fechado   -- Chama a função fazMatriz e passa o parâmetro largura, altura e Fechado
  minas    <- return $ gerarJogo largura altura (40) g    -- Chama a função gerarJogo e passa o parâmetro largura altura nº de bombas e os números aleatórios
  jogar campo minas >>= mostrarMinas                      {- Chama a função jogar e passa o parâmetro campo e minas, >>= passa o resultado da expressão à esquerda 
                                                             como argumento para a expressão à direita, de uma maneira que respeite o contexto do argumento e da função usada-}


jogar :: Campo -> Minas -> IO Campo
jogar e w = do
  mostrarMinas e                                            -- Chama a função mostrarMinas passado o parêmtro e (que é o campo mandado na função main)
  putStrLn "Escolha uma coordenada (x y):" 
  jogada <- getLine
  novoC  <- return $ analisar w (analisaEntrada jogada) e
  putStrLn $ case bombaVisivel novoC of
    True -> "\nVoce perdeu!"
    False -> "\nContinue"
  case bombaVisivel novoC of
      True  -> return novoC
      False -> jogar novoC w


analisar :: Minas -> Coordenada -> Campo -> Campo
analisar w p@(x, y) e = case e !! x !! y of
                            Fechado -> atualizarCampo w p e
                            _       -> e

atualizarCampo :: Minas -> Coordenada -> Campo -> Campo
atualizarCampo w p@(x, y) e =
    case estado of
      Peso 0 -> foldr (analisar w) novoCampo $ emTorno largura altura p
      _      -> novoCampo
    where
      estado    = (w !! x !! y)
      novoCampo = trocarIndiceMatriz p e estado


bombaVisivel :: Campo -> Bool
bombaVisivel [] = False
bombaVisivel (xs:xss)
              | temBomba xs     = True
              | otherwise       = bombaVisivel xss
              where temBomba xs = not $ all naoTemBomba xs
                    naoTemBomba Bomba   = False
                    naoTemBomba _       = True
                                      

analisaEntrada :: String -> Coordenada
analisaEntrada line = listToPair $ map read $ words line
    where listToPair (x:y:_) = (y, x)


gerarJogo :: RandomGen g => Int -> Int -> Int -> g -> Minas
gerarJogo w h n g = [zipWith combinar ms cs | (ms, cs) <- zip bombaMap mapaPeso]
                   where
                     bombas   = nub $ gerarCoordenadas w h n g
                     mapaPeso = geraMapaPeso w h bombas
                     bombaMap = gerarMinas w h n bombas
                     combinar :: (Estado a) -> a -> Estado a
                     combinar Bomba _    = Bomba
                     combinar Fechado _  = Fechado
                     combinar (Peso _) x = Peso x
                                            

gerarCoordenadas :: RandomGen g => Int -> Int -> Int -> g -> [Coordenada]
gerarCoordenadas w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g


gerarMinas :: Int -> Int -> Int -> [Coordenada] -> Minas
gerarMinas w h n bombas = foldr localBomba mina bombas
    where
      mina              = fazMatriz w h (Peso 0)
      localBomba p mina = trocarIndiceMatriz p mina Bomba


geraMapaPeso :: Int -> Int -> [Coordenada] -> MapaPeso
geraMapaPeso w h bombas = foldr succCoordenada mapaPeso emTornoCoordenadas
    where
      emTornoCoordenadas               = concat $ map (emTorno w h) bombas
      mapaPeso                         = replicate w $ replicate h 0
      succCoordenada p@(x, y) mapaPeso = trocarIndiceMatriz p mapaPeso
                                   $ succ (mapaPeso !! x !! y)
                                     

emTorno :: Int -> Int -> Coordenada -> [Coordenada]
emTorno w h (x, y) =
    filter (noLimite w h) [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y),             (x+1, y),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]


mostrarMinas ::  Minas -> IO ()
mostrarMinas w = putStrLn $ mostrarMatrizCom mostrarPonto w

mostrarPonto :: Estado Int -> String
mostrarPonto Bomba      = mostrarCentralizado espaco "*"
mostrarPonto Fechado    = mostrarCentralizado espaco "#"
mostrarPonto (Peso 0)   = mostrarCentralizado espaco " "
mostrarPonto (Peso n)   = mostrarCentralizado espaco (show n)

mostrarCentralizado :: Int -> String -> String
mostrarCentralizado w x = (replicate espacoEsq ' ') ++ x ++ (replicate espacoDir ' ')
    where espacoEsq  =  w `div` 2
          espacoDir  =  w - espacoEsq - (length x)


mapaMatriz :: (a -> b) -> [[a]] -> [[b]]
mapaMatriz f xss = map (map f) xss


mostrarMatrizCom :: (a -> String) -> [[a]] -> String
mostrarMatrizCom f = unlines . adicionaBorda . map concat . mapaMatriz f . transpose


adicionaBorda :: [String] -> [String]
adicionaBorda xs = [numHorizontal]
               ++ [bordaHorizontal w]
               ++ map bordaVertical xs
               ++ [(bordaHorizontal w)]
    where w                  = length (xs !! 0)
          h                  = length xs
          numHorizontal      = "  0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19"
          bordaHorizontal w  = " " ++ (replicate w '-') ++ " "
          bordaVertical xs   = "|" ++ xs ++ "|"


trocarIndiceMatriz :: Coordenada -> [[a]] -> a -> [[a]]
trocarIndiceMatriz (x, y) m e = substituirIndice x m $ substituirIndice y (m !! x) e


substituirIndice :: Int -> [a] -> a -> [a]
substituirIndice indice xs x = take indice xs ++ ( x : (drop (indice+1) xs))


fazMatriz :: Int -> Int -> a -> [[a]]
fazMatriz w h e = replicate w $ replicate h e


noLimite :: Int -> Int -> Coordenada -> Bool
noLimite w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
