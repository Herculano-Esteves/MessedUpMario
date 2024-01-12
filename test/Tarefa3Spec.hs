module Tarefa3Spec (testesTarefa3) where

import LI12324
import Tarefa3
import Test.HUnit

blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 = Personagem (0.0,0.0) Jogador (8.5,7) Oeste (0.8,0.8) False False 10 0 (True, 10.0) False

en1 = Personagem (0.0,0.0) Fantasma (8,7) Este (0.8,0.8) False True 10 0 (False, 0.0) False
en2 = Personagem (0.0,0.0) Fantasma (8.7,7) Este (0.8,0.8) False True 10 0 (False, 0.0) False

c1 = (Martelo, (5,1))

j1 = Jogo gameMap1 [en1,en2] [c1] pl1 3 ((0,0),(0,0)) 0 False

teste1A = "T1A: Inimigo 1 perde vida." ~: True ~=? (vida . head . inimigos $ movimenta 100 1.0 j1) < 10
teste1B = "T1B: Jogador perde vida." ~: True ~=? (vida . jogador $ movimenta 100 1.0 j1) < 10
teste1C = "T1C: Inimigo 2 não perde vida." ~: True ~=? (vida . last . inimigos $ movimenta 100 1.0 j1) == 10

pl2 = Personagem (0.0,0.0) Jogador (5.2,1) Oeste (0.8,0.8) False False 10 0 (False, 0.0) False

j3 = Jogo gameMap1 [] [c1] pl2 3 ((0,0),(0,0)) 0 False

j4 = Jogo gameMap1 [] [] (pl2 {aplicaDano = (True, 10.0)}) 3 ((0,0),(0,0)) 0 False

teste2A = "T2A: Jogador apanha martelo e a flag fica True." ~: True ~=? (fst . aplicaDano . jogador $ movimenta 100 1.0 j3) 
teste2B = "T2B: Jogador apanha martelo e o tempo restante é maior que zero." ~: True ~=? (snd . aplicaDano . jogador $ movimenta 100 1.0 j3) > 0

pl3 = Personagem (0.0,0.0) Jogador (3.5,4) Oeste (0.8,0.8) True False 10 0 (False, 0.0) False

j5 = Jogo gameMap1 [] [] pl3 3 ((0,0),(0,0)) 0 False

teste3 = "T3: Jogador não cai quando esta na escada." ~: j5 ~=? movimenta 100 1.0 j5

pl4 = Personagem (-1.0,0.0) Jogador (0.5,10.5) Oeste (1,1) False False 10 0 (False, 0.0) False

j6 = Jogo gameMap1 [] [] pl4 3 ((0,0),(0,0)) 0 False

teste4 = "T4: Jogador não atravessa o limite do mapa." ~: False ~=? (fst . posicao . jogador $ movimenta 100 1.0 j6) < 0.0

pl5 = Personagem (0.0,0.0) Jogador (5,7.6) Oeste (1,1) False False 10 0 (False, 0.0) False
en3 = Personagem (0.0,0.0) Fantasma (2.5,7.6) Este (1,1) False True 10 0 (False, 0.0) False

j7 = Jogo gameMap1 [en3] [] pl5 3 ((0,0),(0,0)) 0 False

blocos2 :: [[Bloco]]
blocos2 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap2 :: Mapa
gameMap2 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos2

teste5 = "T5: Alcapao e removido por jogador mas nao pelo inimigo." ~: gameMap2 ~=? (mapa $ movimenta 100 1.0 j7)

pl6 = Personagem (0.0,0.0) Jogador (5,1) Oeste (1,1) False False 10 0 (False, 0.0) False
c2 = (Moeda, (5,1))

j8 = Jogo gameMap1 [] [c2] pl6 3 ((0,0),(0,0)) 0 False

teste6 = "T6: Jogador apanha uma moeda" ~: True ~=? (pontos . jogador $ movimenta 100 1.0 j8) > (pontos . jogador $ j8)

testesTarefa3 = test [teste1A, teste1B, teste1C, teste2A, teste2B, teste3, teste4, teste5, teste6]
