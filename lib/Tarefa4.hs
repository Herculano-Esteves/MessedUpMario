{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
    ( Jogo(..),
      Acao(..),
      Personagem(..),
      Direcao(Este, Norte, Sul, Oeste),
      Mapa(..),
      Bloco(..) )
import Tarefa1
import Tarefa2
import Utilities

-- | Função que executa a ação de cada personagem num dado jogo
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza actions action jogo
    | length actions == length (inimigos jogo) = jogo {
        jogador = atualizaPersonagem jogo action (jogador jogo),
        inimigos = atualizaInimigos jogo actions (inimigos jogo)
        }
    | otherwise = jogo


-- Função que aplica a atualizaPersonagem a cada inimigo com a ação respetiva
atualizaInimigos :: Jogo -> [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos jogo actions inms = zipWith (atualizaPersonagem jogo) actions inms


-- * Change the velocity
-- TODO: Define how each character is going to jump
-- | Função que para um dado jogo, ação e personagem, aplica a velocidade correspondente à ação recebida
atualizaPersonagem :: Jogo -> Maybe Acao -> Personagem -> Personagem
atualizaPersonagem jogo action inm = case action of
        Just Subir -> if emEscada inm then
                inm {
                    posicao = (fromIntegral (floor (fst (posicao inm))) + 0.5, snd (posicao inm)),
                    velocidade = (0,-ladderSpeed),
                    direcao = Norte}
            else
                inm -- TODO: Check if it is on the head or last of the stair, so that you can´t go up or down on the start/end of the stairs
        Just Descer -> if canGoDown inm (mapa jogo) then
                inm {
                    posicao = (fromIntegral (floor (fst (posicao inm))) + 0.5, snd (posicao inm)),
                    velocidade = (0,ladderSpeed),
                    direcao = Sul}
            else
                inm
        Just AndarEsquerda -> if not (snd (velocidade inm) == 0) then inm else inm {velocidade = (-4,snd (velocidade inm)), direcao = Oeste}
        Just Saltar -> if (snd (velocidade inm) == 0 ) && not (emEscada inm) || canJump inm (mapa jogo) then
                inm {velocidade = (fst $ (velocidade inm),-5)}
            else
                inm
        Just AndarDireita -> if not (snd (velocidade inm) == 0) then inm else inm {velocidade = (4,snd (velocidade inm)), direcao = Este}
        -- Just Parar -> inm {velocidade = (0,if (emEscada inm) then 0 else snd (velocidade inm))} -- TODO: Make the player stop after releasing key when on ladder
        Just Parar -> if (not $ emEscada inm) then inm {velocidade = (0, snd (velocidade inm))} else inm {velocidade = (0,0)}
        Nothing -> inm
    {-where onTopLadder :: Personagem -> Bool
          onTopLadder perso = floorPos (posicao perso) == head (agrupaEscadas (getPosOfBlock Escada mat))
          (Mapa _ _ mat) = mapa jogo-}

-- | Função que para uma personagem e mapa, devolve um bool correspondente a se pode descer uma escada
canGoDown :: Personagem -> Mapa -> Bool
canGoDown jog (Mapa _ _ blocos)= emEscada jog ||
    (any (\(x,y) -> floorPos (posicao jog) == (x,y-2)) (getPosOfBlock Escada blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y-1)) (getPosOfBlock Plataforma blocos))

canJump :: Personagem -> Mapa -> Bool
canJump jog (Mapa _ _ blocos) = fst (velocidade jog) /= 0 &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y)) (getPosOfBlock Escada blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y-1)) (getPosOfBlock Plataforma blocos)