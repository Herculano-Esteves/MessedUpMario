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
    ( Jogo(inimigos, jogador),
      Acao(..),
      Personagem(velocidade, emEscada, direcao),
      Direcao(Este, Norte, Sul, Oeste) )
import Tarefa1 (dimensaobloco)

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza actions action jogo
    | length actions == length (inimigos jogo) = jogo {
        jogador = atualizaPersonagem jogo action (jogador jogo),
        inimigos = atualizaInimigos jogo actions (inimigos jogo)
        }
    | otherwise = jogo



atualizaInimigos :: Jogo -> [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos jogo actions inms = zipWith (atualizaPersonagem jogo) actions inms

-- * Change the velocity
-- TODO: Define how each character is going to jump
atualizaPersonagem :: Jogo -> Maybe Acao -> Personagem -> Personagem
atualizaPersonagem jogo action inm = case action of
        Just Subir -> if (emEscada inm) then inm {velocidade = (0,-1.2), direcao = Norte} else inm -- TODO: Check if it is on the head or last of the stair, so that you can´t go up or down on the start/end of the stairs
        Just Descer -> if (emEscada inm) then inm {velocidade = (0,1.2), direcao = Sul} else inm
        Just AndarEsquerda -> if not (snd (velocidade inm) == 0) then inm else inm {velocidade = (-4,snd (velocidade inm)), direcao = Oeste}
        Just Saltar -> if (snd (velocidade inm) == 0 ) then inm {velocidade = (fst $ (velocidade inm),-5)} else inm
        Just AndarDireita -> if not (snd (velocidade inm) == 0) then inm else inm {velocidade = (4,snd (velocidade inm)), direcao = Este}
        -- Just Parar -> inm {velocidade = (0,if (emEscada inm) then 0 else snd (velocidade inm))} -- TODO: Make the player stop after releasing key when on ladder
        Just Parar -> inm {velocidade = (0, snd (velocidade inm))}
        Nothing -> inm