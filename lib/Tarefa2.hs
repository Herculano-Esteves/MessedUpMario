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

-- Test data START
enmLs :: [Personagem]
enmLs = [Personagem {ressalta = True, posicao = (20,1.5), tamanho = (7,7)},Personagem {ressalta = True, posicao = (20,1.5), tamanho = (7,7)},Personagem {ressalta = True, posicao = (20,7.5), tamanho = (7,7)}]
jog :: Personagem
jog = Personagem {ressalta = False, posicao = (1.5,2.5), tamanho = (7,7)}
-- Test data END



valida :: Jogo -> Bool
valida = undefined

validaChao :: Mapa -> Bool
validaChao (Mapa _ _ mapMat) = all (== Plataforma) (last mapMat)

-- | Verifica se o ressalto do jogador é falso e se o ressalto de todos os inimigos é verdadeiro
validaRessalta :: Personagem -> [Personagem] -> Bool
validaRessalta jogador inimigosList = not (ressalta jogador) && all ressalta inimigosList

validaPosJogInim :: Personagem -> [Personagem] -> Bool
validaPosJogInim jogador inimigosList = all (==False) (map (\i -> sobreposicao (genHitbox i) (genHitbox jogador)) inimigosList)