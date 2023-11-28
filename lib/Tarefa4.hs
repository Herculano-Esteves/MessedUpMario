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
import Tarefa1 (dimensaobloco)

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza actions action jogo
    | length actions == length (inimigos jogo) = jogo {
        jogador = atualizaPersonagem action (jogador jogo),
        inimigos = atualizaInimigos actions (inimigos jogo)
        }
    | otherwise = jogo



atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos actions inms = zipWith atualizaPersonagem actions inms

-- * Change the velocity
-- TODO: Define how each character is going to jump
atualizaPersonagem :: Maybe Acao -> Personagem -> Personagem
atualizaPersonagem action inm = case action of
        Just Subir -> inm {velocidade = (0,-10), direcao = Norte}
        Just Descer -> inm {velocidade = (0,10), direcao = Sul}
        Just AndarEsquerda -> if (snd (velocidade inm) >0) then inm else inm {velocidade = (-4,0), direcao = Oeste}
        Just Saltar -> if (snd (velocidade inm) == 0 ) then inm {velocidade = (fst $ (velocidade inm),-5)} else inm
        Just AndarDireita -> if (snd (velocidade inm) >0) then inm else inm {velocidade = (4,0), direcao = Este}
        Just Parar -> inm {velocidade = (0,snd (velocidade inm))}
        Nothing -> inm