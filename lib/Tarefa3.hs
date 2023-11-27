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
import GHC.Float (float2Double)


movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed dtime jog = (acionarAlcapao (podeFimDireita (coletarObjetos (perdeVidaJogadorEnd (hitboxDanoJogadorFinal (inimigoMortoEnd (gravidadeQuedaEnd dtime jog)))))))


--Dano Jogador START
hitboxDanoJogadorFinal :: Jogo -> Jogo
hitboxDanoJogadorFinal jogo = jogo {inimigos = hitboxDanoJogador (jogador jogo) (inimigos jogo)}

hitboxDanoJogador :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogador x y   
    | fst (aplicaDano x) && snd (aplicaDano x) > 0 = hitboxDanoJogadoraux x y
    | otherwise = y


hitboxDanoJogadoraux :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogadoraux _ [] = []
hitboxDanoJogadoraux player (h:t)   
    | sobreposicao ((p2-tam1*aux dir,p1),(p4-tam2*aux dir,p3)) (genHitbox h) = h {vida = vida h -1 }: hitboxDanoJogadoraux player t
    | otherwise = h: hitboxDanoJogadoraux player t
    where p1 = snd (fst (genHitbox player))
          p2 = fst (fst (genHitbox player))
          p3 = snd (snd (genHitbox player))
          p4 = fst (snd (genHitbox player))
          tam1 = fst (tamanho player)
          tam2 = snd (tamanho player)
          dir = direcao player
          aux :: Direcao -> Double
          aux x
            | x == Este = -1
            | x == Oeste = 1
            | otherwise = 1
--Dano Jogador END


--Inimigo morto START
inimigoMortoEnd :: Jogo -> Jogo
inimigoMortoEnd jogo = jogo {inimigos = inimigoMorto (inimigos jogo)}

inimigoMorto :: [Personagem] -> [Personagem]
inimigoMorto l = foldl (\x h-> if (vida h == 0) then h {posicao = (-10,-10)} : x else h : x ) [] l
--Inimigo morto END


-- GRAVIDADE START
gravidadeQuedaEnd :: Double -> Jogo -> Jogo
gravidadeQuedaEnd dtime jogo = jogo {inimigos = gravidadeQueda dtime (mapa jogo) (inimigos jogo), jogador = changeVelocidade dtime (mapa jogo) (jogador jogo)}


-- | Muda a gravidade em todas as personagens que precisam de gravidade
gravidadeQueda :: Double -> Mapa -> [Personagem] -> [Personagem]
gravidadeQueda dtime mapa l = foldl (\x y -> x ++ [changeVelocidade dtime mapa y]) [] l

-- | Muda individualmete a gravidade
changeVelocidade :: Double -> Mapa -> Personagem -> Personagem
changeVelocidade dtime mapa perso
    | gravidadeQuedaonoff mapa perso = perso {posicao = ((fst $ (posicao perso)) + (fst $ (velocidade perso))/escalaGloss, (snd $ (posicao perso)) + (snd $ (velocidade perso))*dtime ), velocidade = (fst (velocidade perso),snd (velocidade perso)+(snd gravidade)*dtime) }
    | otherwise = perso {posicao = ((fst $ (posicao perso)) + (fst $ (velocidade perso))*dtime, (snd $ (posicao perso)) + if ((gravidadeQuedaonoff mapa perso)) then (snd $ (velocidade perso))*dtime else 0), velocidade = (fst (velocidade perso), 0)}

-- | Deteta se a gravidade presisa de estar on ou off
gravidadeQuedaonoff :: Mapa -> Personagem -> Bool
gravidadeQuedaonoff mapa perso = all (==False) (map (sobreposicao (genHitbox perso)) (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) mapa))
-- GRAVIDADE END


-- JOGADOR LIFE START
perdeVidaJogadorEnd :: Jogo -> Jogo
perdeVidaJogadorEnd jogo = jogo {jogador = perdeVidaJogador (jogador jogo) (inimigos jogo)}

perdeVidaJogador :: Personagem -> [Personagem] -> Personagem
perdeVidaJogador jog inm
    | all (==False) (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) = jog
    | otherwise = jog {vida = vida jog - 1}
-- JOGADOR LIFE END

-- JOGADOR E OBJETOS START
coletarObjetos :: Jogo -> Jogo
coletarObjetos jogo = jogo {colecionaveis = p1,jogador = (jogador jogo) {pontos = pontos (jogador jogo) + p3,aplicaDano = (if (p4 == False && (snd (aplicaDano (jogador jogo)) > 0)) || p4 then True else False,if p4 && (snd (aplicaDano (jogador jogo)) == 0) then 10 else snd (aplicaDano (jogador jogo)))}}
    where p1 = map fst (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
          p2 = map snd (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
          p3 = sum (map fst p2)
          p4 = not (all (==False) (map snd p2))

coletarObjetosaux :: Personagem -> [(Colecionavel,Posicao)] -> [((Colecionavel,Posicao),(Int,Bool))]
coletarObjetosaux _ [] = []
coletarObjetosaux jog ((x,y):t)
    | estaTocarObjeto jog y = ((x,(-500,-500)),if x == Moeda then (10,False) else (0,True)) : coletarObjetosaux jog t
    | otherwise = ((x,y),(0,False)) : coletarObjetosaux jog t

estaTocarObjeto :: Personagem -> Posicao -> Bool
estaTocarObjeto jog pos = sobreposicao (genHitbox jog) ((fst pos-dimensaobloco*0.5,snd pos+dimensaobloco*0.5),(fst pos+dimensaobloco*0.5,snd pos-dimensaobloco*0.5))
-- JOGADOR E OBJETOS END


--JOGADOR E ALCAPAO START
acionarAlcapao :: Jogo -> Jogo
acionarAlcapao jogo = jogo {mapa = acionarAlcapaoaux (mapa jogo) (jogador jogo)}


acionarAlcapaoaux :: Mapa -> Personagem -> Mapa
acionarAlcapaoaux (Mapa a b c) jog = Mapa a b (alcapaoMapa (dimensaobloco*0.5) c jog)

--funcao que atualiza o mapa double é metade da dimensao do bloco
alcapaoMapa :: Double -> [[Bloco]] -> Personagem -> [[Bloco]]
alcapaoMapa _ [] _ = []
alcapaoMapa x (h:t) jog = alcapaoDifere h (alcapaolinhaAux x (dimensaobloco*0.5) h jog) : alcapaoMapa (x+dimensaobloco) t jog


--funcao que verifica se existem alcapoes perto para desaparecerem tambem
alcapaoDifere :: [Bloco] -> [Bloco] -> [Bloco]
alcapaoDifere b1 b2
    | b1 == b2 = b2
    | otherwise = reverse (alcapaoAux False (reverse b1) (reverse (alcapaoAux False b1 b2)))

alcapaoAux :: Bool -> [Bloco] -> [Bloco] -> [Bloco]
alcapaoAux _ [] [] = []
alcapaoAux b (h:t) (h2:t2)
    | b == False = if h == h2 then h2 : alcapaoAux False t t2 else Vazio : alcapaoAux True t t2
    | b && h2 == Alcapao = Vazio : alcapaoAux True t t2
    | b && not (h==h2) = h2 : alcapaoAux True t t2
    | otherwise = h2 : alcapaoAux False t t2



--funcao que calcula se o jogador toca ou nao numa hitbox de alcapao
alcapaolinhaAux :: Double -> Double -> [Bloco] -> Personagem -> [Bloco]
alcapaolinhaAux _ _ [] _ = []
alcapaolinhaAux y z (h:t) jog
    | h == Alcapao = if sobreposicao (genHitbox jog) (gethitboxbloco dimensaobloco (y,z)) then Vazio : alcapaolinhaAux y (z+dimensaobloco) t jog else h : alcapaolinhaAux y (z+dimensaobloco) t jog
    | otherwise = h : alcapaolinhaAux y (z+dimensaobloco) t jog
--JOGADOR E ALCAPAO END


mapaEmovimentos :: Jogo -> Jogo
mapaEmovimentos jogo = undefined





--Colisoes com parede da direita START
podeFimDireita :: Jogo -> Jogo
podeFimDireita jogo = podeAndarParaADireita jogo (mapa jogo) (jogador jogo)

podeAndarParaADireita :: Jogo -> Mapa -> Personagem -> Jogo
podeAndarParaADireita jogo  mapa ent  
    | not (all (==False) (foldl (\x y -> (sobreposicao ((p1,p2),(p3,p4-0.2)) y) : x) [] (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapa))) || sobreposicao ((p5,p6),(-p7,p8)) ((p1,p2+0.3),(p3,p4-0.3))
        = if fst (velocidade ent) < 0 then  jogo {jogador  = (jogador jogo) {velocidade = (1,snd (velocidade ent))}} else jogo {jogador  = (jogador jogo) {velocidade = (0,snd (velocidade ent))}}
    | otherwise = jogo
    where ((p1,p2),(p3,p4)) = (genEntleftside ent)
          ((p5,p6),(p7,p8)) = (getMapaDimensoes dimensaobloco mapa)

--(getMapaDimensoes dimensaobloco mapa)


getMaprightsideEnd :: [Bloco] -> Mapa -> [Hitbox]
getMaprightsideEnd a b = getMaprightside dimensaobloco a (dimensaobloco*0.5,dimensaobloco*0.5) b

getMaprightside :: Double -> [Bloco] -> Posicao -> Mapa -> [Hitbox]
getMaprightside x l _ (Mapa _ _ []) = []
getMaprightside x l (a,b) (Mapa c d (h:t)) = mapablocosrightside x l (a,b) h ++ getMaprightside x l (a,b+x) (Mapa c d t)

mapablocosrightside :: Double -> [Bloco] -> Posicao -> [Bloco] -> [Hitbox]
mapablocosrightside x l _ [] = []
mapablocosrightside x l (a,b) (h:t)
    | h `elem` l = mapablocosrightside x l (a+x,b) t ++ [gethitboxrightside x (a,b)]
    | otherwise = mapablocosrightside x l (a+x,b) t

gethitboxrightside :: Double -> Posicao -> Hitbox
gethitboxrightside x (a,b) = ((a+(x*0.5),b-(x*0.5)),(a+(x*0.5),b+(x*0.5)))

genEntleftside :: Personagem -> Hitbox
genEntleftside p = (p1,p2)
    where p1 = (xp - fst (tamanho p)/2, yp - snd (tamanho p)/2)
          p2 = (xp - fst (tamanho p)/2, yp + snd (tamanho p)/2)
          xp = fst (posicao p)
          yp = snd (posicao p)
--Colisoes com parede da direita END




{--
chaoPlataformas :: Mapa -> Personagem -> Personagem
chaoPlataformas mapa ent | snd (velocidade ent) > 0 = ent
                         | snd (velocidade ent) == 0 && not (all (==False) (foldl (\x y -> sobreposicao (genEntFloor ent) y : x) [] (getMapFloorEnd [Plataforma,Alcapao] mapaTeste))) = undefined
                         | otherwise = ent




Esta Funcao gera as hitbox só do chao
getMapFloorEnd :: [Bloco] -> Mapa -> [Hitbox]
getMapFloorEnd bloc map = getMapFloor dimensaobloco bloc (dimensaobloco*0.5,dimensaobloco*0.5) map

getMapFloor :: Double -> [Bloco] -> Posicao -> Mapa -> [Hitbox]
getMapFloor x l _ (Mapa _ _ []) = []
getMapFloor x l (a,b) (Mapa c d (h:t)) = mapablocosFloor x l (a,b) h ++ getMapFloor x l (a,b+x) (Mapa c d t)

mapablocosFloor :: Double -> [Bloco] -> Posicao -> [Bloco] -> [Hitbox]
mapablocosFloor x l _ [] = []
mapablocosFloor x l (a,b) (h:t)    | h `elem` l = mapablocosFloor x l (a+x,b) t ++ [gethitboxFloor x (a,b)]
                                    | otherwise = mapablocosFloor x l (a+x,b) t

gethitboxFloor :: Double -> Posicao -> Hitbox
gethitboxFloor x (a,b) = ((a-(x*0.5),b-(x*0.5)),(a+(x*0.5),b-(x*0.5)))

genEntFloor :: Personagem -> Hitbox
genEntFloor p = (p1,p2)
    where p1 = (xp - fst (tamanho p)/2, yp - snd (tamanho p)/2)
          p2 = (xp + fst (tamanho p)/2, yp - snd (tamanho p)/2)
          xp = fst (posicao p)
          yp = snd (posicao p)
--}