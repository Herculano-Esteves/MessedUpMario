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
import GHC.Float (float2Double, double2Float)
import Utilities
import Data.Fixed (mod')
import Mapas
import Text.Read (Lexeme(String))

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed dtime jog    | lostGame jog == 5 = jog
                            | lostGame jog == 2 = perdeVidaJogadorEnd dtime jog
                            | otherwise = setStarPos $ naoPassaPeloTetoFinal dtime $ ladderConditions $ perdeVidaJogadorJogo $ movimentoMacacoMalvado dtime $ checkEscadas (acionarAlcapao (removerPersoChao ( coletarObjetos dtime  (hitboxDanoJogadorFinal (inimigoMortoEnd (movimentoInimigos seed (gravidadeQuedaEnd dtime jog)))))))
                            where (a,b) = aplicaDano (jogador jog)


distancia :: Posicao -> Posicao -> Double
distancia (x,y) (a,b) = sqrt (abs ((x-a)^2+(y-b)^2))

firstDecimal :: Double -> Int
firstDecimal num = floor ((num * 10) - fromIntegral (floor num) * 10)
-- esquerda pertence, direita nao

onlyOneTipoLista :: [Personagem] -> [Entidade] -> ([Personagem],[Personagem])
onlyOneTipoLista lista ent = foldl (\(a,b) y -> if tipo y `elem` ent then (y : a,b) else (a,y : b)) ([],[]) lista

onlyOneTipo :: [Personagem] -> Entidade -> ([Personagem],[Personagem])
onlyOneTipo lista ent = foldl (\(a,b) y -> if tipo y == ent then (y : a,b) else (a,y : b)) ([],[]) lista

--Dano Jogador START
hitboxDanoJogadorFinal :: Jogo -> Jogo
hitboxDanoJogadorFinal jogo | null (inimigos jogo) = jogo
                            | otherwise = jogo {inimigos = hitboxDanoJogador (jogador jogo) (inimigos jogo)}

hitboxDanoJogador :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogador x y
    | fst (aplicaDano x) && snd (aplicaDano x) > 0 = hitboxDanoJogadoraux x y
    | otherwise = y


hitboxDanoJogadoraux :: Personagem -> [Personagem] -> [Personagem]
hitboxDanoJogadoraux _ [] = []
hitboxDanoJogadoraux player (h:t)
    | aux dir == 3 = h:t
    | sobreposicao ((p2-tam1*aux dir,p1),(p4-tam2*aux dir,p3)) (genHitbox h) = h {vida = vida h -1 }: hitboxDanoJogadoraux player t
    | otherwise = h: hitboxDanoJogadoraux player t
    where ((p2,p1),(p4,p3)) = genHitbox player
          (tam1,tam2) = tamanho player
          dir = direcao player
          aux :: Direcao -> Double
          aux x
            | x == Este = -1
            | x == Oeste = 1
            | otherwise = 3
--Dano Jogador END


--Inimigo morto START
inimigoMortoEnd :: Jogo -> Jogo
inimigoMortoEnd jogo = jogo {inimigos = inimigoMorto (inimigos jogo)}

inimigoMorto :: [Personagem] -> [Personagem]
inimigoMorto enm    | null enm = enm
                    | otherwise = foldl (\x h-> if vida h == 0 then h {posicao = (-20,-20)} : x else h : x ) [] enm
--Inimigo morto END


-- GRAVIDADE START
gravidadeQuedaEnd :: Double -> Jogo -> Jogo
gravidadeQuedaEnd dtime jogo = jogo {inimigos = (gravidadeQueda dtime (mapa jogo) sofreGrav) ++ naoSofre, jogador = changeVelocidade dtime (mapa jogo) (jogador jogo)}
                            where   (sofreGrav,naoSofre) = onlyOneTipoLista (inimigos jogo) [EyeEntidade,Fantasma,Barril,MacacoMalvado]


-- | Muda a gravidade em todas as personagens que precisam de gravidade
gravidadeQueda :: Double -> Mapa -> [Personagem] -> [Personagem]
gravidadeQueda dtime mapa = map (changeVelocidade dtime mapa)
-- gravidadeQueda dtime mapa = foldl (\x y -> x ++ [changeVelocidade dtime mapa y]) []

-- | Aplica a velocidade à personagem e aplca a gravidade quando não está no chão
changeVelocidade :: Double -> Mapa -> Personagem -> Personagem
changeVelocidade dtime mapa perso
    | gravidadeQuedaonoff mapa perso = perso {
        posicao = (xPos, snd (posicao perso) + snd (velocidade perso)*dtime ),
        velocidade = (fst (velocidade perso),snd (velocidade perso)+snd gravidade*dtime)
        }
    | otherwise = perso {
        posicao = (xPos, snd (posicao perso) + snd (velocidade perso)*dtime),
        velocidade = velocidade perso
        }
    -- returns the X pos according to certain coditions
    where xPos = if not (podeAndarParaDireitaBool mapa perso) && fst (velocidade perso) < 0 || not (podeAndarParaEsquerdaBool mapa perso) && fst (velocidade perso) > 0 then
                fst (posicao perso) -- get player out of wall (??)
            else
                fst (posicao perso) + fst (velocidade perso)*dtime

-- | Deteta se a gravidade presisa de estar on ou off
gravidadeQuedaonoff :: Mapa -> Personagem -> Bool
gravidadeQuedaonoff mapa perso = not (any (sobreposicao (genHitbox perso)) (getMapColisions 1 [Plataforma,Alcapao,Tunel,Porta] (1*0.5,1*0.5) mapa)) &&
    not (any (\(x,y) -> floorPos (posicao perso) == (x,y)) (getPosOfBlockMap Escada mapa) && fst (velocidade perso) == 0)
-- GRAVIDADE END

-- ! This -0.01 was needed for the player to go up the ladder
seDentroSai :: Mapa -> Personagem -> Personagem
seDentroSai mapa ent | any (sobreposicao ((p1,p4-0.01),(p3,p4-0.01))) (getMapColisions dimensaobloco [Plataforma,Alcapao,Tunel,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa) && not (isOnBlockWithStairBelow ent mapa) =
                    ent {posicao = (fst (posicao ent),fromIntegral (floor p4)-snd (tamanho ent)*0.5),velocidade = (fst (velocidade ent),0)}
                     | otherwise = ent
                    where ((p1,p2),(p3,p4)) = genHitbox ent

removerPersoChao :: Jogo -> Jogo
removerPersoChao jogo = jogo {
    jogador = seDentroSai (mapa jogo) (jogador jogo),
    inimigos = map (\inm -> (if (tipo inm /= Barril) && (tipo inm /= CuspoDeFogo) then seDentroSai (mapa jogo) inm else inm)) (inimigos jogo)
}
--where onFstLadder = any (\)

isOnBlockWithStairBelow :: Personagem -> Mapa -> Bool
isOnBlockWithStairBelow jog (Mapa e j blocos) = any (\(x,y) -> floorPos (posicao jog) == (x,y-2)) (getPosOfBlock Escada blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y-1) || floorPos (posicao jog) == (x,y)) (getPosOfBlock Plataforma blocos) && (snd (velocidade jog) == ladderSpeed || snd (velocidade jog) == -ladderSpeed || emEscada jog)

-- JOGADOR LIFE START
perdeVidaJogadorEnd :: Tempo -> Jogo -> Jogo
perdeVidaJogadorEnd tempo jogo  | lostGame jogo == 2 && animacaoJogo jogo > 0 = jogo {animacaoJogo = animarMorte tempo (animacaoJogo jogo)}
                                | (vida $ jogador jogo) == 0 = jogo {lostGame = 0}
                                | lostGame jogo == 2 && animacaoJogo jogo <= 0 = jogo {lostGame = 4}
                                -- | otherwise = jogoSamp {jogador = (jogador jogo) {posicao = posicao (jogador jogoSamp),aplicaDano = (False,0),temChave = False}}
                                | otherwise = jogo {lostGame = 3}
perdeVidaJogadorJogo :: Jogo -> Jogo
perdeVidaJogadorJogo jogo   | cheatsjogo jogo = jogo
                            | otherwise = jogo {jogador = perdeVidaJogador (mapa jogo) (jogador jogo) (inimigos jogo),animacaoJogo = perdeVidaJogador1 (mapa jogo) (jogador jogo) (inimigos jogo),lostGame = perdeVidaJogador2 (mapa jogo) (lostGame jogo) (jogador jogo) (inimigos jogo)}

-- | Função que verifica se o jogador colide com algum inimigo
perdeVidaJogador :: Mapa -> Personagem -> [Personagem] -> Personagem
perdeVidaJogador mapa jog inm
    | all not (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) && all (==False) (foldl (\x y -> sobreposicao (genHitbox jog) y : x) [] (getMapColisions dimensaobloco [Espinho] (dimensaobloco*0.5,dimensaobloco*1.2) mapa)) = jog
    | otherwise = jog {vida = vida jog - 1}


perdeVidaJogador1 :: Mapa -> Personagem -> [Personagem] -> Float
perdeVidaJogador1 mapa jog inm
    | all not (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) && all (==False) (foldl (\x y -> sobreposicao (genHitbox jog) y : x) [] (getMapColisions dimensaobloco [Espinho] (dimensaobloco*0.5,dimensaobloco*1.2) mapa)) = 0
    | otherwise = 3

perdeVidaJogador2 :: Mapa -> Int -> Personagem -> [Personagem] -> Int
perdeVidaJogador2 mapa n jog inm
    | all not (foldl (\x y -> colisoesPersonagens jog y : x ) [] inm) && all (==False) (foldl (\x y -> sobreposicao (genHitbox jog) y : x) [] (getMapColisions dimensaobloco [Espinho] (dimensaobloco*0.5,dimensaobloco*1.2) mapa)) = n
    | otherwise = 2

animarMorte :: Tempo -> Float -> Float
animarMorte tempo con   | con > 0 = con - double2Float tempo
                        | otherwise = 0

-- JOGADOR LIFE END

-- JOGADOR E OBJETOS START

coletarObjetos :: Tempo -> Jogo -> Jogo
coletarObjetos tempo jogo = jogo {colecionaveis = coletarObjetosremover (colecionaveis jogo) (jogador jogo),jogador = (jogador jogo) {pontos = isMoedaApanhada (filterObjetos (colecionaveis jogo) Moeda) (jogador jogo) (pontos (jogador jogo)),
                                                                                                                                aplicaDano = tempoDoAplicaDano (aplicaDanoFuncao (filterObjetos (colecionaveis jogo) Martelo) (jogador jogo) (aplicaDano (jogador jogo))) tempo,
                                                                                                                                temChave = pegouChave (filterObjetos (colecionaveis jogo) Chave) (jogador jogo) (temChave (jogador jogo))},lostGame = pegouEstrela (lostGame jogo) (filterObjetos (colecionaveis jogo) Estrela) (jogador jogo)}
filterObjetos :: [(Colecionavel,Posicao)] -> Colecionavel -> [(Colecionavel,Posicao)]
filterObjetos [] _ = []
filterObjetos (h:t) obj | fst h == obj = h : filterObjetos t obj
                        | otherwise = filterObjetos t obj


pegouEstrela :: Int -> [(Colecionavel,Posicao)] -> Personagem -> Int
pegouEstrela n lista jog  | estaTocarObjeto jog (snd (head lista)) = 1
                        | otherwise = n

pegouChave :: [(Colecionavel,Posicao)] -> Personagem -> Bool -> Bool
pegouChave [] _ v = v
pegouChave (h:t) jog v  | v = True
                        | estaTocarObjeto jog (snd h) = True
                        | otherwise = pegouChave t jog v

tempoDoAplicaDano :: (Bool,Double) -> Tempo -> (Bool,Double)
tempoDoAplicaDano (a,b) tempo   | b > 15 = (a,b)
                                | b > 0 = (a,b-tempo)
                                | b <= 0 = (False,0)

aplicaDanoFuncao :: [(Colecionavel,Posicao)] -> Personagem -> (Bool,Double) -> (Bool,Double)
aplicaDanoFuncao [] _ (v,i) = (v,i)
aplicaDanoFuncao (h:t) player (v,i) | estaTocarObjeto player (snd h) = (True,10)
                                    | otherwise = aplicaDanoFuncao t player (v,i)

isMoedaApanhada :: [(Colecionavel,Posicao)] -> Personagem -> Int -> Int
isMoedaApanhada obj player ponto = ponto + length (filter id (map (estaTocarObjeto player . snd) obj)) * 10

colecionarIndividualBool :: (Colecionavel,Posicao) -> Personagem -> Bool
colecionarIndividualBool (c,p) player   | estaTocarObjeto player p = True
                                        | otherwise = False

coletarObjetosremover :: [(Colecionavel,Posicao)] -> Personagem -> [(Colecionavel,Posicao)]
coletarObjetosremover itens player = map (`colecionarIndividual` player) itens

colecionarIndividual :: (Colecionavel,Posicao) -> Personagem -> (Colecionavel,Posicao)
colecionarIndividual (c,p) player   | estaTocarObjeto player p = (c,(-20,-20))
                                    | otherwise = (c,p)

estaTocarObjeto :: Personagem -> Posicao -> Bool
estaTocarObjeto jog pos = sobreposicao (genHitbox jog) ((fst pos-dimensaobloco*0.5,snd pos+dimensaobloco*0.5),(fst pos+dimensaobloco*0.5,snd pos-dimensaobloco*0.5))
-- JOGADOR E OBJETOS END


--JOGADOR E ALCAPAO START
acionarAlcapao :: Jogo -> Jogo
acionarAlcapao jogo = jogo {mapa = acionarAlcapaoaux (mapa jogo) (jogador jogo) Alcapao}


acionarAlcapaoaux :: Mapa -> Personagem -> Bloco -> Mapa
acionarAlcapaoaux (Mapa a b c) jog bloco = Mapa a b (removerChao (Mapa a b c) jog bloco)



removerChao :: Mapa -> Personagem -> Bloco -> [[Bloco]]
removerChao (Mapa a b c) jog bloco  | not (any (sobreposicao (genHitbox jog)) (getMapColisions dimensaobloco [bloco] (dimensaobloco*0.5,dimensaobloco*0.5) (Mapa a b c))) = c
                                    | otherwise = removerAlcapao (dimensaobloco*0.5) c jog bloco

removerAlcapao :: Double -> [[Bloco]] -> Personagem -> Bloco -> [[Bloco]]
removerAlcapao _ [] _ _ = []
removerAlcapao x l jog bloco | bloco `elem` head l = removerUmAlcapao x (dimensaobloco*0.5) (head l) jog bloco : removerAlcapao (x+dimensaobloco) (tail l) jog bloco
                             | otherwise = head l : removerAlcapao (x+dimensaobloco) (tail l) jog bloco

removerUmAlcapao :: Double -> Double -> [Bloco] -> Personagem -> Bloco -> [Bloco]
removerUmAlcapao _ _ [] _ _ = []
removerUmAlcapao y x l jog bloco  | (sobreposicao ((px+0.07,p4),(px,p4)) ((px2+0.07,p6),(px2,p6)) || sobreposicao ((p1,p2),(p3,p4))  ((p5,p6),(p7,p8)) && fst (velocidade jog) == 0) && head l == bloco = Vazio : removerUmAlcapao y (x+dimensaobloco) (tail l) jog bloco
                            | otherwise = head l : removerUmAlcapao y (x+dimensaobloco) (tail l) jog bloco
                            where   ((p1,p2),(p3,p4)) = genHitbox jog
                                    ((p5,p6),(p7,p8)) = gethitboxbloco dimensaobloco (x,y)
                                    px = (p1+p3)*0.5
                                    px2 = (p5+p7)*0.5
--ALcapao END


--Logistica de movimento Start
naoPassaPeloTetoFinal :: Tempo -> Jogo -> Jogo
naoPassaPeloTetoFinal tempo jogo = jogo {jogador = naoPassaPeloTeto tempo (mapa jogo) (jogador jogo),inimigos = naoPassaPeloTetoinimigo tempo (mapa jogo) (inimigos jogo)}

naoPassaPeloTetoinimigo :: Tempo -> Mapa -> [Personagem] -> [Personagem]
naoPassaPeloTetoinimigo tempo mapa inm = (map (naoPassaPeloTeto tempo mapa) a) ++ b
                                    where   (a,b) = onlyOneTipoLista inm [EyeEntidade,Fantasma]


naoPassaPeloTeto :: Tempo -> Mapa -> Personagem -> Personagem
naoPassaPeloTeto tempo mapa jogador   | emEscada jogador = jogador
                                | elem True (foldl (\x y -> sobreposicao ((p1,p2),(p3,p2-0.1)) y : x) [] (getMapColisions dimensaobloco [Plataforma,Tunel,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) = jogador {velocidade = (fst (velocidade jogador),0),posicao = (fst (posicao jogador),snd (posicao jogador)+2*tempo)}
                                | otherwise = jogador
                    where ((p1,p2),(p3,p4)) = genHitbox jogador


podeAndarParaEsquerdaBool :: Mapa -> Personagem -> Bool
podeAndarParaEsquerdaBool mapa ent = all not (foldl (\x y -> sobreposicao ((p3+0.1,p2+0.2),(p3,p4-0.2)) y : x) [] (getMapColisions dimensaobloco [Plataforma,Tunel,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa++
                                    getMapColisions dimensaobloco [Porta] (dimensaobloco*0.5,dimensaobloco*1.5) mapa++
                                    getMapColisions dimensaobloco [Espinho] (dimensaobloco*0.5,dimensaobloco*1) mapa)) && not (sobreposicao ((p8+1,p6),(p8,p7)) ((p1,p2),(p3,p4)))
    where ((p1,p2),(p3,p4)) = genHitbox ent
          ((p5,p6),(p7,p8)) = getMapaDimensoes dimensaobloco mapa


podeAndarParaDireitaBool :: Mapa -> Personagem -> Bool
podeAndarParaDireitaBool mapa ent = all not (foldl (\x y -> sobreposicao ((p1-0.1,p2+0.2),(p1,p4-0.2)) y : x) [] ((getMapColisions dimensaobloco [Plataforma,Tunel,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)++
                                    getMapColisions dimensaobloco [Porta] (dimensaobloco*0.5,dimensaobloco*1.5) mapa++
                                    getMapColisions dimensaobloco [Espinho] (dimensaobloco*0.5,dimensaobloco*1) mapa)) && not (sobreposicao ((0,0),(-p8,p7)) ((p1,p2),(p3,p4)))
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
--Logistica de movimento End





-- Ladder logic started
checkEscadas :: Jogo -> Jogo
checkEscadas jogo = jogo {
    inimigos = checkEscadaList (mapa jogo) (inimigos jogo),
    jogador = checkEscadaAux (mapa jogo) (jogador jogo)
}

checkEscadaList :: Mapa -> [Personagem] -> [Personagem]
checkEscadaList mapa = map (checkEscadaAux mapa)

checkEscadaAux :: Mapa -> Personagem -> Personagem
checkEscadaAux (Mapa _ _ mat) perso = perso {emEscada = any (\pos -> floorPos (posicao perso) == pos) (getPosOfBlock Escada mat) || any (\(x,y) -> floorPos (posicao perso) == (x,y-1)) (getPosOfBlock Escada mat) && any (\(x,y) -> floorPos (posicao perso) == (x,y)) (getPosOfBlock Plataforma mat)}

ladderConditions :: Jogo -> Jogo
ladderConditions jog = jog {
    jogador =
            if (emEscada (jogador jog) &&
                (fst $ velocidade $ jogador jog) == 0 &&
                (snd $ velocidade $ jogador jog) /= 0 &&
                abs (snd $ velocidade $ jogador jog) /= ladderSpeed ) then
                (jogador jog) {
                    velocidade = (0,0),
                    posicao = (fromIntegral (floor (fst (posicao (jogador jog)))) + 0.5, fromIntegral (floor (snd (posicao (jogador jog)))) +0.5),
                    direcao = Norte
                }
            else
                (jogador jog) {
                    velocidade = velocidade $ jogador jog
                }
    }


--INICIO DE AI


movimentoInimigos :: Semente -> Jogo -> Jogo
movimentoInimigos sem jogo = jogo {inimigos = movimentoInimigoscontrolo (geraAleatorios sem (length (inimigos jogo))) (mapa jogo) (inimigos jogo) jogo}

-- movimentoInimigoscontrolo ::[Int] -> Mapa -> [Personagem] -> Jogo -> [Personagem]
-- movimentoInimigoscontrolo _ _ [] _ = []
-- movimentoInimigoscontrolo [] _ _ _ = []
-- movimentoInimigoscontrolo (h:t) mapa (a:b) jogo = if tipo a == Fantasma then inimigoMove h mapa a : movimentoInimigoscontrolo t mapa b jogo else a : movimentoInimigoscontrolo t mapa b jogo

movimentoInimigoscontrolo :: [Int] -> Mapa -> [Personagem] -> Jogo -> [Personagem]
movimentoInimigoscontrolo seeds mapa enms jogo = zipWith (\h a -> if tipo a == Fantasma || tipo a == EyeEntidade then inimigoMove h mapa a else a) seeds enms

inimigoMove :: Int -> Mapa -> Personagem -> Personagem
inimigoMove start mapa enm  | read (take 3 (show start)) <= 304 && read (take 3 (show start)) >= 301 && (emEscada enm || canGoDown' enm mapa) = inimigosubirdescerescada start mapa enm -- colar depois no True (mod (read(take 2 (show start))) 3 == 0 && p)
                            | inimigosubirdescerescadaBool mapa enm = enm
                            | otherwise = inimigoAndar start mapa enm
                            where p = any (sobreposicao (genHitbox enm)) (getMapColisions dimensaobloco [Escada] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)


-- | Função que faz com que o inimigo ande
inimigoAndar :: Int -> Mapa -> Personagem -> Personagem
inimigoAndar start mapa enm     | posicao enm == (-20,-20) = enm
                                | (fst (velocidade enm) == 0) = if start > 0 then enm {velocidade = (1.5,snd (velocidade enm))} else enm {velocidade = (-1.5,snd (velocidade enm))}
                                | not (podeAndarParaEsquerdaBool mapa enm) = enm {velocidade = (-1.5,snd (velocidade enm))}
                                | not (podeAndarParaDireitaBool mapa enm) = enm {velocidade = (1.5,snd (velocidade enm))}
                                | all not (foldl (\x y -> sobreposicao ((p1,p4),(p1-0.1,p4-0.5)) y : x) [] (getMapColisions dimensaobloco [Plataforma,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) = enm {velocidade = (1.5,snd (velocidade enm))}
                                | all not (foldl (\x y -> sobreposicao ((p3,p4),(p3+0.1,p4-0.5)) y : x) [] (getMapColisions dimensaobloco [Plataforma,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) = enm {velocidade = (-1.5,snd (velocidade enm))}
                                | otherwise = enm {velocidade = (fst (velocidade enm),0)}
                                where ((p1,p2),(p3,p4)) = genHitbox enm
                                      p = (any (sobreposicao (genHitbox enm)) (getMapColisions dimensaobloco [Plataforma,Tunel,Alcapao,Porta,Espinho] (dimensaobloco*0.5,dimensaobloco*0.5) mapa))

-- | Função que faz com que o inimigo suba ou desça a escada
inimigosubirdescerescada :: Int -> Mapa -> Personagem -> Personagem
inimigosubirdescerescada start mapa enm --if any (sobreposicao ((p1+0.3,p4),(p3-0.3,p4))) (getMapColisions dimensaobloco [Vazio] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)
                                            --then enm {velocidade = (if fst (velocidade enm) == 0 then 1.5 else fst (velocidade enm),0), posicao = (fst (posicao enm),fromInteger (floor (snd (posicao enm)))+0.5)}
    | canGoDown' enm mapa && snd (velocidade enm) >= 0 || snd (velocidade enm) == ladderSpeed = enm {
                                                posicao = (fromIntegral (floor (fst (posicao enm))) + 0.5, snd (posicao enm)),
                                                velocidade = (0,ladderSpeed)}
    | onFstLadder enm mapa = enm {
                                posicao = (fromIntegral (floor (fst (posicao enm))) + 0.5, snd (posicao enm)),
                                velocidade = (0,-ladderSpeed)}
    | otherwise = enm
    where ((p1,p2),(p3,p4)) = genHitbox enm

inimigosubirdescerescadaBool :: Mapa -> Personagem -> Bool
inimigosubirdescerescadaBool mapa enm = fst (velocidade enm) == 0 && (snd (velocidade enm) == -ladderSpeed || snd (velocidade enm) == ladderSpeed) -- && (emEscada enm || canGoDown' enm mapa))

-- | Função que verifica se o inimigo está em cima de uma escada
onFstLadder :: Personagem -> Mapa -> Bool
onFstLadder jog (Mapa _ _ blocos) = any (\(x,y) -> floorPos (posicao jog) == (x,y-1)) (getPosOfBlock Plataforma blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y)) (getPosOfBlock Escada blocos)


canGoDown' :: Personagem -> Mapa -> Bool
canGoDown' jog (Mapa _ _ blocos)= any (\(x,y) -> floorPos (posicao jog) == (x,y-2)) (getPosOfBlock Escada blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y-1) ) (getPosOfBlock Plataforma blocos) ||
    any (\(x,y) -> floorPos (posicao jog) == (x,y-1)) (getPosOfBlock Escada blocos) &&
    any (\(x,y) -> floorPos (posicao jog) == (x,y)) (getPosOfBlock Plataforma blocos)



--Macaco Malvado Start
-- Só pode existir um macacomalvado por mapa
movimentoMacacoMalvado :: Tempo -> Jogo -> Jogo
movimentoMacacoMalvado tempo jogo   | MacacoMalvado `elem` map tipo (inimigos jogo) = jogo {inimigos = movimentaBarris (mapa jogo) tempo (animaMacacoMalvado (mapa jogo) tempo (jogador jogo) a) c ++ d}
                                    | otherwise = jogo
                                    where   (a,b) = onlyOneTipo (inimigos jogo) MacacoMalvado
                                            (c,d) = onlyOneTipo b Barril

--sabendo que só pode existir um macaco malvado no mapa
animaMacacoMalvado :: Mapa -> Tempo -> Personagem -> [Personagem] -> Personagem
animaMacacoMalvado mapa tempo jogador macaco = enm{posicao = 
     if fst(posicao jogador) > fst(posicao enm)+0.2 && podeAndarParaEsquerdaBool mapa enm  then (fst(posicao enm)+(tempo*1.5),snd(posicao enm))
     else if fst(posicao jogador)+0.2 < fst(posicao enm) && podeAndarParaDireitaBool mapa enm then (fst(posicao enm)-(tempo*1.5),snd(posicao enm)) else posicao enm
                                            ,direcao = if fst(posicao jogador) < fst(posicao enm)-0.3 then Este else if fst(posicao jogador) > fst(posicao enm)+0.3 then Oeste else Norte
                                            ,aplicaDano = if snd (aplicaDano enm) <= 0 then (True,15) else (snd (aplicaDano enm) == 15, snd (aplicaDano enm)-tempo)}
                                        where enm = head macaco

movimentaBarris :: Mapa -> Tempo -> Personagem -> [Personagem] -> [Personagem]
movimentaBarris mapa tempo macaco lista | snd (aplicaDano macaco) == 15 = macaco : [barrilpersonagem{posicao = posicao macaco,velocidade = (0,1)}]
                                        | null lista = [macaco]
                                        | otherwise = macaco : foldl (\x y -> if sobreposicao ((a,b),(c,d)) (genHitbox y) then movimentaBarrisaux ((a,b),(c,d)) tempo y macaco ++ x else x) [] lista
                                        where ((a,b),(c,d)) = getMapaDimensoes 1 mapa


movimentaBarrisaux :: Hitbox ->  Tempo -> Personagem -> Personagem -> [Personagem]
movimentaBarrisaux map tempo barril macaco  | not (sobreposicao map (genHitbox barril)) = []
                                        | vida barril <= 0 || (fst (posicao barril) > 0 && fst (posicao barril) < 0) = [barril {posicao = (-5,-5),velocidade = (0,0),vida = 1}]
                                        | snd (aplicaDano macaco) == 15 = [barril {posicao = posicao macaco,velocidade = (0,1)}]
                                        | otherwise = [barril {posicao = (fst (posicao barril), snd (posicao barril) + snd (velocidade barril)*tempo)}]
--Macaco Malvado End

setStarPos :: Jogo -> Jogo
setStarPos jog = jog {
    colecionaveis = map (\(a,b) -> if a == Estrela then (a, posf) else (a,b)) (colecionaveis jog)
}
    where (Mapa _ posf _) = mapa jog