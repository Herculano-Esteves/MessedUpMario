{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1
import Tarefa2

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta = undefined




hitboxDanoJogador :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogador x y   | fst (aplicaDano x) && snd (aplicaDano x) > 0 = hitboxDanoJogadoraux x y
                        | otherwise = y


hitboxDanoJogadoraux :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogadoraux _ [] = []
hitboxDanoJogadoraux player (h:t)   | sobreposicao ((p2-tam1*aux dir,p1),(p4-tam2*aux dir,p3)) (genHitbox h) = h {vida = vida h -1 }: hitboxDanoJogadoraux player t
                                    | otherwise = h: hitboxDanoJogadoraux player t

                                where   p1 = snd (fst (genHitbox player))
                                        p2 = fst (fst (genHitbox player))
                                        p3 = snd (snd (genHitbox player))
                                        p4 = fst (snd (genHitbox player))
                                        tam1 = fst (tamanho player)
                                        tam2 = snd (tamanho player)
                                        dir = direcao player
                                        aux :: Direcao -> Double
                                        aux x   | x == Este = -1
                                                | x == Oeste = 1
                                                | otherwise = 1
                                        