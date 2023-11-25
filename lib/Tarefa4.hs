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

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza = undefined

atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos actions inms = zipWith (\a b -> b) actions inms

-- * Change the velocity
-- TODO: Define how each character is going to jump
atualizaInimigo :: Maybe Acao -> Personagem -> Personagem
atualizaInimigo action inm = case action of
        Just Subir -> inm {velocidade = (0,-10)}
        Just Descer -> inm {velocidade = (0,10)}
        Just AndarEsquerda -> inm {velocidade = (-10,0), direcao = Oeste}
        Just AndarDireita -> inm {velocidade = (10,0), direcao = Este}
        Just Parar -> inm {velocidade = (0,0)}
        Nothing -> inm