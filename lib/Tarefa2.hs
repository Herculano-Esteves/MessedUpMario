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
import Data.List (elemIndex, elemIndices, groupBy, sortOn)
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
valida jogo = validaChao (mapa jogo) &&
    validaRessalta (jogador jogo) (inimigos jogo) &&
    validaPosJogInim (jogador jogo) (inimigos jogo) &&
    validaNumIniAndVidaFan (inimigos jogo) &&
    validaEscadas (mapa jogo) &&
    validaAlcapoes (mapa jogo) &&
    validaPosPersColecs jogo

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
validaNumIniAndVidaFan inis = (length inis >= 2) && (all (\f -> vida f == 1) $ filter (\p -> tipo p == Fantasma) inis)

-- | Verfica se as escadas são continuas e terminam e começam com plataforma
-- | 
validaEscadas :: Mapa -> Bool
validaEscadas (Mapa _ _ mat) = all validateEachOne (agrupaEscadas (getPosOfBlock Escada mat))
    where validateEachOne :: [Posicao] -> Bool
          validateEachOne ls = ((x1,y1-1) `elem` getPosOfBlock Plataforma mat &&
                                (x2,y2-1) `elem` getPosOfBlock Escada mat) ||
                                ((x2,y2+1) `elem` getPosOfBlock Plataforma mat &&
                                (x1,y1+1) `elem` getPosOfBlock Escada mat)
            where (x1,y1) = head ls
                  (x2,y2) = last ls


agrupaEscadas :: [Posicao] -> [[Posicao]]
agrupaEscadas pos = map (\p-> [head p] ++ [last p]) $ agrupaEscadasAux (groupEscadasAux pos)

agrupaEscadasAux :: [Posicao] -> [[Posicao]]
agrupaEscadasAux [] = []
agrupaEscadasAux [x] = [[x]]
agrupaEscadasAux ((x,y):t)
    | elem (x,y+1) (head r) = ((x,y) : (head r)) : tail r
    | otherwise = [(x,y)] : r
    where r = agrupaEscadasAux t

groupEscadasAux :: [Posicao] -> [Posicao]
groupEscadasAux pos = sortOn fst pos

-- ? Maybe this should return the pos of a block as (0.5,0.5) instead of (0,0), however we would have to refactor the code
-- | Retorna as posições de todosos blocos de um certo tipo num dado mapa
getPosOfBlock :: Bloco -> [[Bloco]] -> [Posicao]
getPosOfBlock bloco mat = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! double2Int y !! double2Int x == bloco]

-- ! Reminder to remove this
getPosOfBlock' :: Bloco -> Mapa -> [Posicao]
getPosOfBlock' bloco (Mapa _ _ mat) = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! double2Int y !! double2Int x == bloco]

-- TODO: Discuss the size of blocks and player, needed for the 7th step
-- | Verifica se os alçapões se encontram pelo menos em grupos de 2
validaAlcapoes :: Mapa -> Bool
validaAlcapoes (Mapa _ _ mat) = all (\(x,y) -> (x+1,y) `elem` matEsc || (x-1,y) `elem` matEsc) matEsc
    where matEsc = getPosOfBlock Alcapao mat

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