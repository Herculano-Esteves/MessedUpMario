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


inimigoMorto :: [Personagem] -> [Personagem]
inimigoMorto l = foldl (\x h-> if (vida h == 0) then h {posicao = (-10,-10)} : x else h : x ) [] l

-- GRAVIDADE START
-- | Muda a gravidade em todas as personagens que precisam de gravidade
gravidadeQueda :: Mapa -> [Personagem] -> [Personagem]
gravidadeQueda mapa l = foldl (\x y -> x ++ [changeVelocidade mapa y]) [] l

-- | Muda individualmete a gravidade
changeVelocidade :: Mapa -> Personagem -> Personagem
changeVelocidade mapa perso     | gravidadeQuedaonoff mapa perso = perso {velocidade = (fst (velocidade perso),snd gravidade) }
                                | otherwise = perso

-- | Deteta se a gravidade presisa de estar on ou off
gravidadeQuedaonoff :: Mapa -> Personagem -> Bool
gravidadeQuedaonoff mapa perso = all (==False) (map (sobreposicao (genHitbox perso)) (getMapColisions 10 [Plataforma] (5,5) mapa))
-- GRAVIDADE END


-- JOGADOR LIFE START
perdeVidaJogador :: Personagem -> [Personagem] -> Personagem
perdeVidaJogador jog inm        | all (==False) (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) = jog
                                | otherwise = jog {vida = vida jog - 1}
-- JOGADOR LIFE END

-- JOGADOR E OBJETOS START
coletarObjetos :: Jogo -> Jogo
coletarObjetos jogo = jogo {colecionaveis = p1,jogador = (jogador jogo) {pontos = pontos (jogador jogo) + p3,aplicaDano = (if (p4 == False && (snd (aplicaDano (jogador jogo)) > 0)) || p4 then True else False,if p4 && (snd (aplicaDano (jogador jogo)) == 0) then 10 else snd (aplicaDano (jogador jogo)))}}
                        where   p1 = map fst (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
                                p2 = map snd (coletarObjetosaux (jogador jogo) (colecionaveis jogo))
                                p3 = sum (map fst p2)
                                p4 = not (all (==False) (map snd p2))
coletarObjetosaux :: Personagem -> [(Colecionavel,Posicao)] -> [((Colecionavel,Posicao),(Int,Bool))]
coletarObjetosaux _ [] = []
coletarObjetosaux jog ((x,y):t) | estaTocarObjeto jog y = ((x,(-5,-5)),if x == Moeda then (10,False) else (0,True)) : coletarObjetosaux jog t
                                | otherwise = ((x,y),(0,False)) : coletarObjetosaux jog t

estaTocarObjeto :: Personagem -> Posicao -> Bool
estaTocarObjeto jog pos = sobreposicao (genHitbox jog) ((snd pos+1,fst pos+1),pos)
-- JOGADOR E OBJETOS END


--JOGADOR E ALCAPAO START
acionarAlcapao :: Jogo -> Jogo
acionarAlcapao jogo = undefined


acionarAlcapaoaux :: Mapa -> Personagem -> Mapa
acionarAlcapaoaux mapa jog = undefined

--funcao que atualiza o mapa double é metade da dimensao do bloco
alcapaoMapa :: Double -> [[Bloco]] -> Personagem -> [[Bloco]]
alcapaoMapa _ [] _ = []
alcapaoMapa x (h:t) jog = alcapaoDifere h (alcapaolinhaAux x (dimensaobloco*0.5) h jog) : alcapaoMapa (x+dimensaobloco) t jog


--funcao que verifica se existem alcapoes perto para desaparecerem tambem
alcapaoDifere :: [Bloco] -> [Bloco] -> [Bloco]
alcapaoDifere b1 b2     | b1 == b2 = b2
                        | otherwise = reverse (alcapaoAux False (reverse b1) (reverse (alcapaoAux False b1 b2)))
                                
alcapaoAux :: Bool -> [Bloco] -> [Bloco] -> [Bloco]
alcapaoAux _ [] [] = []
alcapaoAux b (h:t) (h2:t2)      | b == False = if h == h2 then h2 : alcapaoAux False t t2 else Vazio : alcapaoAux True t t2
                                | b && h2 == Alcapao = Vazio : alcapaoAux True t t2
                                | b && not (h==h2) = h2 : alcapaoAux True t t2
                                | otherwise = h2 : alcapaoAux False t t2



--funcao que calcula se o jogador toca ou nao numa hitbox de alcapao
alcapaolinhaAux :: Double -> Double -> [Bloco] -> Personagem -> [Bloco]
alcapaolinhaAux _ _ [] _ = []
alcapaolinhaAux y z (h:t) jog        | h == Alcapao = if sobreposicao (genHitbox jog) (gethitboxbloco dimensaobloco (y,z)) then Vazio : alcapaolinhaAux y (z+dimensaobloco) t jog else h : alcapaolinhaAux y (z+dimensaobloco) t jog
                                | otherwise = h : alcapaolinhaAux y (z+dimensaobloco) t jog




-- gethitboxbloco :: Double -> Posicao -> Hitbox