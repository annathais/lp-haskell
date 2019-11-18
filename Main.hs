{-
  Universidade de Brasilia
  Curso Engenharia de Computação
  Disciplina Ligugem de Programação   Turma C
  Equipe: Anna Thais Costa Lopes            18/0112279
          Aécio Fernandes Galiza Magalhães  15/0115121
-}

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
  putStrLn "\n----------------------------am----------------------------------"
  putStrLn "--            --> Bem vindo ao Campo Minado <--             --"
  putStrLn "--------------------------------------------------------------\n"
  g        <- getStdGen                                         -- Obtém o gerador global de números aleatórios.
  campo    <- return $ fazMatriz largura altura Fechado         -- Chama a função fazMatriz e passa o parâmetro largura, altura e Fechado
  minas    <- return $ gerarJogo largura altura (40) g          -- Chama a função gerarJogo e passa o parâmetro largura altura nº de bombas e os números aleatórios
  jogar campo minas >>= mostrarMinas                            {- Chama a função jogar e passa o parâmetro campo e minas, >>= passa o resultado da expressão à esquerda 
                                                                   como argumento para a expressão à direita, de uma maneira que respeite o contexto do argumento e da função usada-}


jogar :: Campo -> Minas -> IO Campo
jogar e w = do
  mostrarMinas e                                                -- Chama a função mostrarMinas passado o parêmtro e (que é o campo mandado na função main)
  putStrLn "Escolha uma coordenada (x y):"
  jogada <- getLine                                             -- Captura a jogada
  novoC  <- return $ analisar w (analisaEntrada jogada) e       -- Converte a string de entrada "jogada" em um par de coordenada
  putStrLn $ case bombaVisivel novoC of                         -- Caso tenha uma bomba visivel apos a jogadada,
    True -> "\nVoce perdeu!"                                    -- a mensagem diz que voce perde.
    False -> "\nContinue"                                       -- Caso contrario, continua
  case bombaVisivel novoC of                                    -- Mesmo caso de cima, mas com a atualizacao do tabuleiro
      True  -> return novoC                                     -- Caso perca, imprime o tabuleiro atualizado atual com a bomba visivel
      False -> jogar novoC w                                    -- Caso contrario, retorna para a funcao de jogar


analisar :: Minas -> Coordenada -> Campo -> Campo               -- Analisa se a jogada foi em uma coordenada ja aberta
analisar w p@(x, y) e = case e !! x !! y of                     -- Caso a coordenada ainda esteja fechada,
                            Fechado -> atualizarCampo w p e     -- chama a funcao para atualizar o campo, abrindo-a
                            _       -> e                        -- caso ja esteja aberta, retorna o proprio campo

atualizarCampo :: Minas -> Coordenada -> Campo -> Campo         -- Atualiza o campo impresso
atualizarCampo w p@(x, y) e =
    case estado of
      Peso 0 -> foldr (analisar w) novoCampo $ emTorno largura altura p   -- Caso seja uma coordenada de peso 0 (sem bombas adjacentes), analisa os adjacentes
      _      -> novoCampo                                                 -- ao achar algum que nao tenha peso 0, imprime novo campo
    where
      estado    = (w !! x !! y)
      novoCampo = trocarIndiceMatriz p e estado                           -- onde o novo campo possui um caracter referente ao peso != 0


bombaVisivel :: Campo -> Bool                                   -- verifica se tem bomba visivel apos atualizar o campo
bombaVisivel [] = False                                         -- campo vazio = falso, nenhuma bomba viivel
bombaVisivel (xs:xss)
              | temBomba xs     = True                          -- Se o estado do campo for Bomba, aparece uma bomba
              | otherwise       = bombaVisivel xss              -- aplica recursao pro restante da lista novamente
              where temBomba xs = not $ all naoTemBomba xs
                    naoTemBomba Bomba   = False                 -- Se o estado do campo for Bomba, retora "falso" para "nao tem bomba", o que retorna "verdedeiro" para "tem bomba"
                    naoTemBomba _       = True                  -- em outro caso, nao tem bomba mesmo.


analisaEntrada :: String -> Coordenada
analisaEntrada line = listToPair $ map read $ words line        -- Converte a string de entrada "line"
    where listToPair (x:y:_) = (y, x)                           -- Em uma lista com um par, que será a coordenada


gerarJogo :: RandomGen g => Int -> Int -> Int -> g -> Minas
gerarJogo w h n g = [zipWith combinar ms cs | (ms, cs) <- zip bombaMap mapaPeso] -- Concatena as posicoes das bombas com as posicoes dos pesos, fechando o campo por completo
                   where
                     bombas   = nub $ gerarCoordenadas w h n g  -- remove coordenadas duplicadas
                     mapaPeso = geraMapaPeso w h bombas         -- gera o mapa com os pesos
                     bombaMap = gerarMinas w h n bombas         -- gera o mapa só com as minas
                     combinar :: (Estado a) -> a -> Estado a
                     combinar Bomba _    = Bomba                -- coloca o estado "bomba" em todos que possuirem esse atributo, independente do nome
                     combinar Fechado _  = Fechado              -- faz o mesmo com "fechado"
                     combinar (Peso _) x = Peso x               -- faz o mesmo com peso


gerarCoordenadas :: RandomGen g => Int -> Int -> Int -> g -> [Coordenada]
gerarCoordenadas w h n g = zip xs ys                            -- concatena os x's e y's na forma de coordenada
    where
      xs = take n (randomRs (0, w-1) g)                         -- pega 40 x's aleatorios entre 0 e tamanho do eixo X
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g            -- pega 2 vezes o tamanho (40) e dropa metade


gerarMinas :: Int -> Int -> Int -> [Coordenada] -> Minas
gerarMinas w h n bombas = foldr localBomba mina bombas          -- aplica "localbomba" colocando o estado da matriz "mina" onde for bomba efetivamente
    where
      mina              = fazMatriz w h (Peso 0)                -- cria matriz com esses parametros
      localBomba p mina = trocarIndiceMatriz p mina Bomba


geraMapaPeso :: Int -> Int -> [Coordenada] -> MapaPeso                      -- gera o mapa apenas com os pesos
geraMapaPeso w h bombas = foldr succCoordenada mapaPeso emTornoCoordenadas  -- aplica a operacao "succCoordenada" por mapaPeso nas coordenadas adjacentes
    where
      emTornoCoordenadas               = concat $ map (emTorno w h) bombas  -- concatena as listas com as bombas e os pesos aplicados em torno
      mapaPeso                         = replicate w $ replicate h 0        -- replica "w" vezes a replica de "h" vezes 0 (pesos iniciais)
      succCoordenada p@(x, y) mapaPeso = trocarIndiceMatriz p mapaPeso      -- funcao "succCoordenada"
                                   $ succ (mapaPeso !! x !! y)


emTorno :: Int -> Int -> Coordenada -> [Coordenada]             -- mapeia coordenadas em torno
emTorno w h (x, y) =
    filter (noLimite w h) [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y),             (x+1, y),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]


mostrarMinas ::  Minas -> IO ()                                 -- funcao de imprimir o campo
mostrarMinas w = putStrLn $ mostrarMatrizCom mostrarPonto w

mostrarPonto :: Estado Int -> String                            -- funcao que imprime a situacao atual da coordenada
mostrarPonto Bomba      = mostrarCentralizado espaco "*"
mostrarPonto Fechado    = mostrarCentralizado espaco "#"
mostrarPonto (Peso 0)   = mostrarCentralizado espaco " "
mostrarPonto (Peso n)   = mostrarCentralizado espaco (show n)

mostrarCentralizado :: Int -> String -> String                                          -- Apresenta valor na matriz centralizado, separado pelo espaço definido no início
mostrarCentralizado l x = (replicate espacoEsq ' ') ++ x ++ (replicate espacoDir ' ')   -- Gera uam String com o espaço a esquerda, o conteúdo e o espaço a direita
    where espacoEsq  =  l `div` 2                                                       -- Espaço a esquerda é o número de vezes o espaço pode ser dividido por 2
          espacoDir  =  l - espacoEsq - (length x)                                      -- Espaço a direira é o espaço menos o espaço a esquerda menos o tamanho do conteudo da celula


mapaMatriz :: (a -> b) -> [[a]] -> [[b]]                        -- Mapeia uma função sobre uma lista de listas
mapaMatriz f xss = map (map f) xss


mostrarMatrizCom :: (a -> String) -> [[a]] -> String            -- Cria uma string a partir de uma matriz de strings, insere novos caracteres de linha entre as strings originais
mostrarMatrizCom f = unlines . adicionaBorda . map concat . mapaMatriz f . transpose 


adicionaBorda :: [String] -> [String]
adicionaBorda xs = [numHorizontal]                              -- Essa função recebe a String xs e retorna uma sString maior composta pelos números da borda superior,
               ++ [bordaHorizontal l]                           -- linha da borda superior, linha da borda lateral e a matriz, seja esta #, ou os pesos (1,2,3), ou ainda
               ++ map bordaVertical xs                          -- a bomba e termina preenchedo a linha da outra lateral e a inferior
               ++ [(bordaHorizontal l)]
    where l                  = length (xs !! 0)                 -- A largura é do tamanho do comprimento da posição 0 de xs 
          a                  = length xs                        -- A altura é do tamanho do comprimento de xs
          numHorizontal      = "  0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19"
          bordaHorizontal l  = " " ++ (replicate l '-') ++ " "
          bordaVertical xs   = "|" ++ xs ++ "|"


trocarIndiceMatriz :: Coordenada -> [[a]] -> a -> [[a]]         -- Substitui o celula da matriz indica pelas coordenadas pelo valor correspondente
trocarIndiceMatriz (x, y) m e = substituirIndice x m $ substituirIndice y (m !! x) e


substituirIndice :: Int -> [a] -> a -> [a]                      -- Substitui um elemento em um índice em uma lista por outro elemento
substituirIndice indice xs x = take indice xs ++ ( x : (drop (indice+1) xs))  



fazMatriz :: Int -> Int -> a -> [[a]]                          -- Cria um preenchimento de matriz com um determinado elemento
fazMatriz l a e = replicate l $ replicate a e                  -- Cria uma lista de comprimento fornecida pelo primeiro argumento e os itens com valor do segundo argumento


noLimite :: Int -> Int -> Coordenada -> Bool                   -- Verifica se a coordenada digitada está dentro do campo
noLimite l a (x, y)
    | x < 0     = False
    | x >= l    = False
    | y < 0     = False
    | y >= a    = False
    | otherwise = True
