{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Nuno Miguel Paiva Fernandes <a107317@alunos.uminho.pt>
              Pedro Herculano Soares Oliveira do Lago Esteves <a106839@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.

IMPORTANTE -> Tudo com IMPORTANTE tem dimensoes do bloco do mapa, possivel alteraçoes
-}
module Tarefa1 where

import LI12324
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float, double2Int)

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
    posicao = (5,15),
    tamanho = (7,7)
}


per1 = Personagem {
    posicao = (35,20),
    tamanho = (10,10)
}

-- | caso a personagem esteja fora do mapa a personagem esta a colidir com as paredes externas

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede mapa perso = not (sobreposicao (genHitbox perso) (getMapaDimensoes mapa)) || foldl (\valor lista -> if valor == True then True else if valor == lista then False else True) False (map (sobreposicao (genHitbox perso)) (getMapColisions (5,5) mapa))

-- | dimensoes do mapa IMPORTANTE a assumir que a dimensao de cada bloco é 10x10
getMapaDimensoes :: Mapa -> Hitbox
getMapaDimensoes (Mapa _ _ (h:t)) = ((0,0),(fromIntegral(length (h:t))*10,fromIntegral(length h)*10))

-- |IMPORTANDE depende de dimensoes do bloco
getMapColisions :: Posicao -> Mapa -> [Hitbox]
getMapColisions _ (Mapa _ _ []) = []
getMapColisions (a,b) (Mapa c d (h:t)) = mapablocoshitbox (a,b) h ++ getMapColisions (a,b+10) (Mapa c d t)

--IMPORTANTE depende da dimensao do bloco 10x10
mapablocoshitbox :: Posicao -> [Bloco] -> [Hitbox]
mapablocoshitbox _ [] = []
mapablocoshitbox (a,b) (h:t)    | h == Plataforma = mapablocoshitbox (a+10,b) t ++ [gethitboxbloco (a,b)]
                                | otherwise = mapablocoshitbox (a+10,b) t


--assumindo que a IMPORTANTE dimensao do bloco é 10x10
gethitboxbloco :: Posicao -> Hitbox
gethitboxbloco (a,b) = ((a+5,b-5),(a-5,b+5))



colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = sobreposicao (genHitbox p1) (genHitbox p2)

-- | A partir de uma personagem gera a hitbox correspondente
genHitbox :: Personagem -> Hitbox
genHitbox p = (p1,p2)
    where p1 = (xp - fst (tamanho p)/2, yp - snd (tamanho p)/2)
          p2 = (xp + fst (tamanho p)/2, yp + snd (tamanho p)/2)
          xp = fst (posicao p)
          yp = snd (posicao p)

-- | verifica se duas hitboxes estão sobrepostas independentemente do seu tamanho
sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao h1 h2= sobreposicaoAux h1 h2 || sobreposicaoAux h2 h1

-- | verifica se duas hitboxes estão sobrepostas (porém só funciona se h2 for menor que h1)
sobreposicaoAux :: Hitbox -> Hitbox -> Bool
sobreposicaoAux ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) = pointInBox (double2Float x3,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x3,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    



                                                