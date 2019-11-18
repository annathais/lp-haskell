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

mostrarCentralizado :: Int -> String -> String                                          -- Apresenta valor na matriz centralizado, separado pelo espaço definido no início
mostrarCentralizado l x = (replicate espacoEsq ' ') ++ x ++ (replicate espacoDir ' ')   -- Gera uam String com o espaço a esquerda, o conteúdo e o espaço a direita
    where espacoEsq  =  l `div` 2                                                       -- Espaço a esquerda é o número de vezes o espaço pode ser dividido por 2
          espacoDir  =  l - espacoEsq - (length x)                                      -- Espaço a direira é o espaço menos o espaço a esquerda menos o tamanho do conteudo da celula


mapaMatriz :: (a -> b) -> [[a]] -> [[b]]      -- Mapeia uma função sobre uma lista de listas
mapaMatriz f xss = map (map f) xss


mostrarMatrizCom :: (a -> String) -> [[a]] -> String    -- Cria uma string a partir de uma matriz de strings, insere novos caracteres de linha entre as strings originais
mostrarMatrizCom f = unlines . adicionaBorda . map concat . mapaMatriz f . transpose 


adicionaBorda :: [String] -> [String]
adicionaBorda xs = [numHorizontal]              -- Essa função recebe a String xs e retorna uma sString maior composta pelos números da borda superior,
               ++ [bordaHorizontal l]           -- linha da borda superior, linha da borda lateral e a matriz, seja esta #, ou os pesos (1,2,3), ou ainda
               ++ map bordaVertical xs          -- a bomba e termina preenchedo a linha da outra lateral e a inferior
               ++ [(bordaHorizontal l)]
    where l                  = length (xs !! 0) -- A largura é do tamanho do comprimento da posição 0 de xs 
          a                  = length xs        -- A altura é do tamanho do comprimento de xs
          numHorizontal      = "  0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19"
          bordaHorizontal l  = " " ++ (replicate l '-') ++ " "
          bordaVertical xs   = "|" ++ xs ++ "|"


trocarIndiceMatriz :: Coordenada -> [[a]] -> a -> [[a]]         -- Substitui o celula da matriz indica pelas coordenadas pelo valor correspondente
trocarIndiceMatriz (x, y) m e = substituirIndice x m $ substituirIndice y (m !! x) e


substituirIndice :: Int -> [a] -> a -> [a]                      -- Substitui um elemento em um índice em uma lista por outro elemento
substituirIndice indice xs x = take indice xs ++ ( x : (drop (indice+1) xs))  



fazMatriz :: Int -> Int -> a -> [[a]]           -- Cria um preenchimento de matriz com um determinado elemento
fazMatriz l a e = replicate l $ replicate a e   -- Cria uma lista de comprimento fornecida pelo primeiro argumento e os itens com valor do segundo argumento


noLimite :: Int -> Int -> Coordenada -> Bool -- Verifica se a coordenada digitada está dentro do campo
noLimite l a (x, y)
    | x < 0     = False
    | x >= l    = False
    | y < 0     = False
    | y >= a    = False
    | otherwise = True
