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
movimenta seed dtime jog = acionarAlcapao (removerjogChao ( coletarObjetos (perdeVidaJogadorEnd (hitboxDanoJogadorFinal (inimigoMortoEnd  (gravidadeQuedaEnd dtime jog))))))


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
inimigoMorto = foldl (\x h-> if (vida h == 0) then h {posicao = (-10,-10)} : x else h : x ) []
--Inimigo morto END


-- GRAVIDADE START
gravidadeQuedaEnd :: Double -> Jogo -> Jogo
gravidadeQuedaEnd dtime jogo = jogo {inimigos = gravidadeQueda dtime (mapa jogo) (inimigos jogo), jogador = changeVelocidade dtime (mapa jogo) (jogador jogo)}


-- | Muda a gravidade em todas as personagens que precisam de gravidade
gravidadeQueda :: Double -> Mapa -> [Personagem] -> [Personagem]
gravidadeQueda dtime mapa = foldl (\x y -> x ++ [changeVelocidade dtime mapa y]) []

-- | Aplica a velocidade à personagem e aplca a gravidade quando não está no chão
changeVelocidade :: Double -> Mapa -> Personagem -> Personagem
changeVelocidade dtime mapa perso
    | gravidadeQuedaonoff mapa perso = perso {
        posicao = (xPos, (snd (posicao perso)) + (snd (velocidade perso))*dtime ),
        velocidade = (fst (velocidade perso),snd (velocidade perso)+snd gravidade*dtime)
        }
    | otherwise = perso {
        posicao = (xPos, (snd (posicao perso)) + (snd (velocidade perso))*dtime),
        velocidade = (fst (velocidade perso), min (snd (velocidade perso)) 0) -- this if resets the Y speed after falling
        }
    -- returns the X pos according to certain coditions
    where xPos = if (not (podeAndarParaDireitaBool mapa perso) && (fst $ velocidade perso) < 0) || (not (podeAndarParaEsquerdaBool mapa perso) && (fst $ (velocidade perso)) > 0) then
                fst $ (posicao perso) -- get player out of wall (??)
            else
                (fst (posicao perso)) + fst (velocidade perso)*dtime

-- | Deteta se a gravidade presisa de estar on ou off
gravidadeQuedaonoff :: Mapa -> Personagem -> Bool
gravidadeQuedaonoff mapa perso = all not (map (sobreposicao (genHitbox perso)) (getMapColisions 1 [Plataforma,Alcapao] (1*0.5,1*0.5) mapa) ++
    map (sobreposicao (genHitbox perso)) (getMapColisions 1 [Escada] (1*0.5,1*0.5) mapa))
-- GRAVIDADE END


removerjogChao :: Jogo -> Jogo
removerjogChao jog = jog {jogador = seDentroSai (mapa jog) (jogador jog)}

seDentroSai :: Mapa -> Personagem -> Personagem
seDentroSai mapa ent | not (all ((==False) . sobreposicao ((p1,p2),(p3,p4))) (getMapColisions dimensaobloco [Plataforma,Alcapao] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) =
                    ent {posicao = (fst (posicao ent),fromIntegral (floor p4)-snd (tamanho ent)*0.5)}
                     | otherwise = ent
                    where ((p1,p2),(p3,p4)) = genHitbox ent




-- JOGADOR LIFE START
perdeVidaJogadorEnd :: Jogo -> Jogo
perdeVidaJogadorEnd jogo = jogo {jogador = perdeVidaJogador (jogador jogo) (inimigos jogo)}

perdeVidaJogador :: Personagem -> [Personagem] -> Personagem
perdeVidaJogador jog inm
    | all not (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) = jog
    | otherwise = jog {vida = vida jog - 1}
-- JOGADOR LIFE END

-- JOGADOR E OBJETOS START
coletarObjetos :: Jogo -> Jogo
coletarObjetos jogo = jogo {colecionaveis = p1,jogador = (jogador jogo) {pontos = pontos (jogador jogo) + p3,aplicaDano = ((p4 == False && (snd (aplicaDano (jogador jogo)) > 0)) || p4,if p4 && snd (aplicaDano (jogador jogo)) == 0 then 10 else snd (aplicaDano (jogador jogo)))}}
    where p1 = map fst (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
          p2 = map snd (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
          p3 = sum (map fst p2)
          p4 = not (all ((==False) . snd) p2)

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
acionarAlcapaoaux (Mapa a b c) jog = (Mapa a b (removerChao (Mapa a b c) jog))


removerChao :: Mapa -> Personagem -> [[Bloco]]
removerChao (Mapa a b c) jog    | (all (==False) (map (sobreposicao (genHitbox jog)) (getMapColisions dimensaobloco [Alcapao] (dimensaobloco*0.5,dimensaobloco*0.5) (Mapa a b c)))) = c
                                | otherwise = removerAlcapao (dimensaobloco*0.5) c jog

removerAlcapao :: Double -> [[Bloco]] -> Personagem -> [[Bloco]]
removerAlcapao _ [] _ = []
removerAlcapao x l jog  | Alcapao `elem` head l = removerUmAlcapao x (dimensaobloco*0.5) (head l) jog : removerAlcapao (x+dimensaobloco) (tail l) jog
                        | otherwise = head l : removerAlcapao (x+dimensaobloco) (tail l) jog

removerUmAlcapao :: Double -> Double -> [Bloco] -> Personagem -> [Bloco]
removerUmAlcapao _ _ [] _ = []
removerUmAlcapao y x l jog  | sobreposicao ((px+0.1,p4),(px,p4)) ((px2+0.1,p6),(px2,p6)) && head l == Alcapao = Vazio : removerUmAlcapao y (x+dimensaobloco) (tail l) jog
                            | otherwise = head l : removerUmAlcapao y (x+dimensaobloco) (tail l) jog
                            where   ((p1,p2),(p3,p4)) = (genHitbox jog)
                                    ((p5,p6),(p7,p8)) = (gethitboxbloco dimensaobloco (x,y))
                                    px = (p1+p3)*0.5
                                    px2 = (p5+p7)*0.5
--ALcapao END



podeAndarParaEsquerdaBool :: Mapa -> Personagem -> Bool
podeAndarParaEsquerdaBool mapa ent = all (==False) (foldl (\x y -> (sobreposicao ((p3+0.1,p2-0.1),(p3,p4-0.2)) y) : x) [] (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) && not (sobreposicao ((p8+1,p6),(p8,p7)) ((p1,p2),(p3,p4)))
    where ((p1,p2),(p3,p4)) = genHitbox ent
          ((p5,p6),(p7,p8)) = getMapaDimensoes dimensaobloco mapa


podeAndarParaDireitaBool :: Mapa -> Personagem -> Bool
podeAndarParaDireitaBool mapa ent = all (==False) (foldl (\x y -> (sobreposicao ((p1-0.1,p2),(p1,p4-0.2)) y) : x) [] (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) && not (sobreposicao ((0,0),(-p8,p7)) ((p1,p2),(p3,p4)))
    where ((p1,p2),(p3,p4)) = genHitbox ent
          ((p5,p6),(p7,p8)) = getMapaDimensoes dimensaobloco mapa

getMaprightsideEnd :: [Bloco] -> Mapa -> [Hitbox]
getMaprightsideEnd a = getMaprightside dimensaobloco a (dimensaobloco*0.5,dimensaobloco*0.5)

getMaprightside :: Double -> [Bloco] -> Posicao -> Mapa -> [Hitbox]
getMaprightside x l _ (Mapa _ _ []) = []
getMaprightside x l (a,b) (Mapa c d (h:t)) = mapablocosrightside x l (a,b) h ++ getMaprightside x l (a,b+x) (Mapa c d t)

mapablocosrightside :: Double -> [Bloco] -> Posicao -> [Bloco] -> [Hitbox]
mapablocosrightside x l _ [] = []
mapablocosrightside x l (a,b) (h:t)
    | h `elem` l = mapablocosrightside x l (a+x,b) t ++ [gethitboxrightside x (a,b)]
    | otherwise = mapablocosrightside x l (a+x,b) t

gethitboxrightside :: Double -> Posicao -> Hitbox
gethitboxrightside x (a,b) = ((a+x*0.5,b-x*0.5),(a+x*0.5,b+x*0.5))
--Colisoes com parede da direita END


isOnFloor :: Jogo -> Bool
isOnFloor jogo = isOnFlooraux  (jogador jogo) (mapa jogo)

isOnFlooraux :: Personagem -> Mapa -> Bool
isOnFlooraux jog mapa = not (all ((==False) . sobreposicao ((p3,p2),(p3,p4))) (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapa))
                        where ((p1,p2),(p3,p4)) = genHitbox jog


--INICIO DE AI
movimentoInimigos :: Semente -> Jogo -> Jogo
movimentoInimigos sem jogo = undefined

-- Ladder logic started
checkEscadas :: Jogo -> Jogo
checkEscadas jogo = jogo {
    inimigos = checkEscadaList (mapa jogo) (inimigos jogo),
    jogador = checkEscadaAux (mapa jogo) (jogador jogo)
}
    where Mapa _ _ mat = mapa jogo
          startEndEscadas = agrupaEscadas (getPosOfBlock Escada mat)

checkEscadaList :: Mapa -> [Personagem] -> [Personagem]
checkEscadaList mapa = map (checkEscadaAux mapa)

checkEscadaAux :: Mapa -> Personagem -> Personagem
checkEscadaAux (Mapa _ _ mat) perso = perso {emEscada = floorPos (posicao perso) `elem` getPosOfBlock Escada mat}
