{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1 (sobreposicao, genHitbox, mapaTeste)
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromMaybe)
import GHC.Float (double2Int)

-- Test data START
inm :: [Personagem]
inm = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 0, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]
jog :: Personagem
jog = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (1.5,5.5), 
                    tamanho = (7,7), 
                    aplicaDano = (True, 10), 
                    direcao = Este}

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda,(0,5)),(Martelo,(7,5))]

jogoSamp ::Jogo
jogoSamp = Jogo mapaTeste inm colec jog
-- Test data END

valida :: Jogo -> Bool
valida = undefined

-- * Add to valida
-- | Verifica o chao do mapa
validaChao :: Mapa -> Bool
validaChao (Mapa _ _ mapMat) = all (== Plataforma) (last mapMat)

-- * Add to valida
-- | Verifica se o ressalto do jogador é falso e se o ressalto de todos os inimigos é verdadeiro
validaRessalta :: Personagem -> [Personagem] -> Bool
validaRessalta jogador inimigosList = not (ressalta jogador) && all ressalta inimigosList

-- * Add to valida
-- | Verifica a posiçao inicial se sobrepoem ou nao com os inimigos
validaPosJogInim :: Personagem -> [Personagem] -> Bool
validaPosJogInim jogador inimigosList = all (\i -> posicao i /= posicao jogador) inimigosList


-- * Add to valida
-- | Verfica se existem pelo menos 2 inimigos e se cada fantasma tem apenas 1 vida
validaNumIniAndVidaFan :: [Personagem] -> Bool
validaNumIniAndVidaFan inis = (length inis == 2) && (all (\f -> vida f == 1) $ filter (\p -> tipo p == Fantasma) inis)

-- * Add to valida
-- TODO: Check if this should check for the platform blocks arround the end/start of the platform
-- | Verfica se as escadas são continuas e terminam e começam com plataforma
validaEscadas :: Mapa -> Bool
validaEscadas (Mapa _ _ mat) = all (\(x,y) -> ((x,y-1) `elem` getPosOfBlock Plataforma mat || (x,y-1) `elem` getPosOfBlock Escada mat) && ((x,y+1) `elem` getPosOfBlock Plataforma mat || (x,y+1) `elem` getPosOfBlock Escada mat)) (getPosOfBlock Escada mat)


-- ? Maybe this should return the pos of a block as (0.5,0.5) instead of (0,0), however we would have to refactor the code
-- | Retorna as posições de todosos blocos de um certo tipo num dado mapa
getPosOfBlock :: Bloco -> [[Bloco]] -> [Posicao]
getPosOfBlock bloco mat = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! double2Int y !! double2Int x == bloco]

-- ! Reminder to remove this
getPosOfBlock' :: Bloco -> Mapa -> [Posicao]
getPosOfBlock' bloco (Mapa _ _ mat) = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! double2Int y !! double2Int x == bloco]


-- TODO: Discuss the size of blocks and player, needed for the 7th step

-- * Add to valida
-- | Verifica se os colecionaveis se encontram em espaços vazios no mapa e se as personagens se encontram em espaços vazios do mapa
validaPosPersColecs :: Jogo -> Bool
validaPosPersColecs jogo = validaPosPers (jogador jogo) (inimigos jogo) (mapa jogo) && validaColecs (colecionaveis jogo) (mapa jogo) 

-- | Verifica se os colecionávei se encontram em espaços vazios do mapa
validaColecs :: [(Colecionavel,Posicao)] -> Mapa -> Bool
validaColecs colecs (Mapa _ _ mat) = all (\(c,(x,y)) -> (fromIntegral $ floor x, fromIntegral $ floor y) `elem` getPosOfBlock Vazio mat) colecs

-- | Verifica se as personagens (jogador e inimigos) se encontram em espaços vazios do mapa
validaPosPers :: Personagem -> [Personagem] -> Mapa -> Bool
validaPosPers player inms (Mapa _ _ mat) = floorPos (posicao player) `elem` getPosOfBlock Vazio mat && all (\inm -> floorPos (posicao inm) `elem` getPosOfBlock Vazio mat) inms
    where floorPos :: Posicao -> Posicao
          floorPos (x,y) = (fromIntegral $ floor x, fromIntegral $ floor y)