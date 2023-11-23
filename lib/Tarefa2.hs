{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1 (sobreposicao, genHitbox)
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromMaybe)
import GHC.Float (double2Int)

-- Test data START
enmLs :: [Personagem]
enmLs = [Personagem {ressalta = True, posicao = (1.5,1.5), tamanho = (7,7)},Personagem {ressalta = True, posicao = (20,1.5), tamanho = (7,7)},Personagem {ressalta = True, posicao = (20,7.5), tamanho = (7,7)}]
jog :: Personagem
jog = Personagem {ressalta = False, posicao = (1.5,2.5), tamanho = (7,7)}
-- Test data END



valida :: Jogo -> Bool
valida = undefined

-- | Verifica o chao do mapa
validaChao :: Mapa -> Bool
validaChao (Mapa _ _ mapMat) = all (== Plataforma) (last mapMat)

-- | Verifica se o ressalto do jogador é falso e se o ressalto de todos os inimigos é verdadeiro
validaRessalta :: Personagem -> [Personagem] -> Bool
validaRessalta jogador inimigosList = not (ressalta jogador) && all ressalta inimigosList

-- | Verifica a posiçao inicial se sobrepoem ou nao com os inimigos
validaPosJogInim :: Personagem -> [Personagem] -> Bool
validaPosJogInim jogador inimigosList = all (\i -> posicao i /= posicao jogador) inimigosList


-- | Verfica se existem pelo menos 2 inimigos e se cada fantasma tem apenas 1 vida
validaNumIniAndVidaFan :: [Personagem] -> Bool
validaNumIniAndVidaFan inis = (length inis == 2) && (all (\f -> vida f == 1) $ filter (\p -> tipo p == Fantasma) inis)

-- TODO: Check if this should check for the platform blocks arround the end/start of the platform
-- | Verfica se as escadas são continuas e terminam e começam com plataforma
validaEscadas :: Mapa -> Bool
validaEscadas (Mapa _ _ mat) = all (\(x,y) -> ((x,y-1) `elem` getPosOfBlock Plataforma mat || (x,y-1) `elem` getPosOfBlock Escada mat) && ((x,y+1) `elem` getPosOfBlock Plataforma mat || (x,y+1) `elem` getPosOfBlock Escada mat)) (getPosOfBlock Escada mat)

-- | Retorna as posições de todosos blocos de um certo tipo num dado mapa
getPosOfBlock :: Bloco -> [[Bloco]] -> [Posicao]
getPosOfBlock bloco mat = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! double2Int y !! double2Int x == bloco]

