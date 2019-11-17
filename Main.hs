module Main where

import Control.Monad
import System.Random
import Data.List
import System.IO
import Debug.Trace
import Tipos


espaco   = 3
largura  = 20
altura = 20

main :: IO ()
main = do
  g        <- getStdGen
  campo <- return $ fazMatriz largura altura Fechado -- nothing is campo
  minas    <- return $ gerarJogo largura altura (largura * altura `div` 10) g
  jogar campo minas >>= mostrarMinas


jogar :: Campo -> Minas -> IO Campo
jogar e w = do
  mostrarMinas e
  putStrLn "Escolha uma coordenada (x,y):" 
  jogada <- getLine
  newC <- return $ analisar w (parseInput jogada) e
  case bombaVisivel newC of
      True  -> return newC
      False -> jogar newC w

analisar :: Minas -> Coordenada -> Campo -> Campo
analisar w p@(x, y) e = case e !! x !! y of
                         Fechado -> atualizarCampo w p e
                         _          -> e

atualizarCampo :: Minas -> Coordenada -> Campo -> Campo
atualizarCampo w p@(x, y) e =
    case estado of
      Peso 0 -> foldr (analisar w) novoCampo $ emTorno largura altura p
      _      -> novoCampo
    where
      estado       = (w !! x !! y)
      novoCampo = trocarIndiceMatriz p e estado


bombaVisivel :: Campo -> Bool
bombaVisivel [] = False
bombaVisivel (xs:xss)
              | temBomba xs = True
              | otherwise       = bombaVisivel xss
              where temBomba xs = not $ all naoTemBomba xs
                    naoTemBomba Bomba    = False
                    naoTemBomba _       = True
                                      

parseInput :: String -> Coordenada
parseInput line = listToPair $ map read $ words line
    where listToPair (x:y:_) = (y, x)


gerarJogo :: RandomGen g => Int -> Int -> Int -> g -> Minas
gerarJogo w h n g = [zipWith combinar ms cs | (ms, cs) <- zip bombaMap mapaPeso]
                   where
                     bombas   = nub $ gerarCoordenadas w h n g
                     mapaPeso = geraMapaPeso w h bombas
                     bombaMap = gerarMinas w h n bombas
                     combinar :: (Estado a) -> a -> Estado a
                     combinar Bomba _       = Bomba
                     combinar Fechado _ = Fechado
                     combinar (Peso _) x   = Peso x
                                            

gerarCoordenadas :: RandomGen g => Int -> Int -> Int -> g -> [Coordenada]
gerarCoordenadas w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g


gerarMinas :: Int -> Int -> Int -> [Coordenada] -> Minas
gerarMinas w h n bombas = foldr placeBomba mina bombas
    where
      mina             = fazMatriz w h (Peso 0) -- Initial mina has no bombas
      placeBomba p mina = trocarIndiceMatriz p mina Bomba


geraMapaPeso :: Int -> Int -> [Coordenada] -> MapaPeso
geraMapaPeso w h bombas = foldr succCoordenada mapaPeso emTornoCoordenadas
    where
      emTornoCoordenadas          = concat $ map (emTorno w h) bombas
      mapaPeso                    = replicate w $ replicate h 0
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
mostrarPonto Bomba       = mostrarCentralizado espaco "*"
mostrarPonto Fechado = mostrarCentralizado espaco "#"
mostrarPonto (Peso 0)   = mostrarCentralizado espaco " "
mostrarPonto (Peso n)   = mostrarCentralizado espaco (show n)

mostrarCentralizado :: Int -> String -> String
mostrarCentralizado w x = (replicate espacoEsq ' ') ++ x ++ (replicate espacoDir ' ')
    where espacoEsq  =  w `div` 2
          espacoDir =  w - espacoEsq - (length x)


mapaMatriz :: (a -> b) -> [[a]] -> [[b]]
mapaMatriz f xss = map (map f) xss

mostrarMatrizCom :: (a -> String) -> [[a]] -> String
mostrarMatrizCom f = unlines . adicionaBorda . map concat . mapaMatriz f . transpose


adicionaBorda :: [String] -> [String]
adicionaBorda xs = [bordaHorizontal w]
               ++ map bordaVertical xs
               ++ [(bordaHorizontal w)]
    where w                  = length (xs !! 0)
          h                  = length xs
          bordaHorizontal w = "+" ++ (replicate w '-') ++ "+"
          bordaVertical xs  = "|" ++ xs ++ "|"

trocarIndiceMatriz :: Coordenada -> [[a]] -> a -> [[a]]
trocarIndiceMatriz (x, y) m e = substituirIndice x m $ substituirIndice y (m !! x) e


substituirIndice :: Int -> [a] -> a -> [a]
substituirIndice index xs x = take index xs ++ ( x : (drop (index+1) xs))


fazMatriz :: Int -> Int -> a -> [[a]]
fazMatriz w h e = replicate w $ replicate h e

noLimite :: Int -> Int -> Coordenada -> Bool
noLimite w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
