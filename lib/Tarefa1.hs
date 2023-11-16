{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float)

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede = undefined

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens = undefined


{-
type Hitbox = (Posicao, Posicao)

-- | Vetor velocidade.
type Velocidade = (Double, Double)

-- | Posicao no 'Mapa'.
type Posicao = (Double, Double)
-}

sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao p1 p2= sobreposicaoAux p1 p2 || sobreposicaoAux p2 p1

-- fazer função que faz: sobreposicao h1 h2 || sobreposicao h2 h1 (funciona para se as hitboxes tiverem tamanhos diferentes)
sobreposicaoAux :: Hitbox -> Hitbox -> Bool
sobreposicaoAux ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) = pointInBox (double2Float x3,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x3,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    



                                                