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

mapaTeste = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
    [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

per = Personagem {
    posicao = (20,20),
    tamanho = (10,10)
}


per1 = Personagem {
    posicao = (35,20),
    tamanho = (10,10)
}

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede = undefined



colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = sobreposicao (genHitbox p1) (genHitbox p2)


genHitbox :: Personagem -> Hitbox
genHitbox p = (p1,p2)
    where p1 = (xp - fst (tamanho p)/2, yp - snd (tamanho p)/2)
          p2 = (xp + fst (tamanho p)/2, yp + snd (tamanho p)/2)
          xp = fst (posicao p)
          yp = snd (posicao p)

sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao h1 h2= sobreposicaoAux h1 h2 || sobreposicaoAux h2 h1

-- fazer função que faz: sobreposicao h1 h2 || sobreposicao h2 h1 (funciona para se as hitboxes tiverem tamanhos diferentes)
sobreposicaoAux :: Hitbox -> Hitbox -> Bool
sobreposicaoAux ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) = pointInBox (double2Float x3,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x3,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    



                                                