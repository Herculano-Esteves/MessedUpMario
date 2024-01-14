module Extras where

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Mapas
import Utilities

extrasFuncao :: Semente -> Tempo -> State -> State
extrasFuncao random tempo state = state{levels = replace (levels state) (currentLevel state,(movimentaExtras state tempo random (screenSize state) jogo, unlocked))}
                            where (jogo, unlocked) = levels state !! currentLevel state



movimentaExtras :: State -> Tempo -> Semente -> (Int,Int) -> Jogo -> Jogo
movimentaExtras state dtime random screen jogo  | lostGame jogo == 5 = jogo
                                                | lostGame jogo == 2 = perdeVidaJogadorEnd dtime jogo
                                                | otherwise = caoMovimenta random dtime $ atiradorMovimento random dtime $ canhaoMovimento random dtime $ portasFuncao $ movimentaCuspoJogo dtime $ eyeentityMovimento dtime $ eyebossMovimento dtime $ cheatModeAtualiza (cheats state) $ cameraHitbox screen dtime $ bossMovimento dtime jogo

--Cheats START
-- | Controla se os cheats estao ativos ou nao (nao colide com os inimigos)
cheatModeAtualiza :: Bool -> Jogo -> Jogo
cheatModeAtualiza condicao jogo = jogo {cheatsjogo= condicao}
--Cheats END


--Contrlo de Camera
-- | Controla a camera de modo que só se veja o mapa
cameraHitbox :: (Int,Int) -> Tempo -> Jogo -> Jogo
cameraHitbox (a,b) tempo jogo = jogo {cameraControl = cameraHitboxaux (a,b) (mapa jogo) tempo (jogador jogo) (cameraControl jogo) }

cameraHitboxaux :: (Int,Int) -> Mapa -> Tempo -> Personagem -> Hitbox -> Hitbox
cameraHitboxaux (h,t) mapa tempo jog hit = controlarCameraHitbox 4 mapa (h,t) ((a,b),(c,d)) $ cameraHitboxaux4 tempo (cameraHitboxaux3 tempo (cameraHitboxaux2 tempo (cameraHitboxaux1 tempo hit ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)
                     where  ((ta,tb),(ba,bb)) = hit
                            ((tx,ty),(bx,by)) = ((z-1,w-1),(z+1,w+1))
                            (z,w) = posicao jog
                            (vx,vy) = (if abs (fst (velocidade jog)) == 0 then 1 else abs (fst (velocidade jog)),if abs (snd (velocidade jog)) == 0 then 1 else abs (snd (velocidade jog)))
                            ((a,b),(c,d)) = getMapaDimensoes 1 mapa
-- | Condiçoes da camera
cameraHitboxaux1 :: Tempo -> Hitbox -> Hitbox -> Velocidade-> Hitbox
cameraHitboxaux1 tempo hit jog (vx,vy)  | ta > tx = ((ta-vx*tempo,tb),(ba-vx*tempo, bb))
                                        | otherwise = hit
                            where   ((ta,tb),(ba,bb)) = hit
                                    ((tx,ty),(bx,by)) = jog
cameraHitboxaux2 :: Tempo -> Hitbox -> Hitbox -> Velocidade-> Hitbox
cameraHitboxaux2 tempo hit jog (vx,vy)  | ba < bx = ((ta+vx*tempo,tb),(ba+vx*tempo, bb))
                                        | otherwise = hit
                            where   ((ta,tb),(ba,bb)) = hit
                                    ((tx,ty),(bx,by)) = jog
cameraHitboxaux3 :: Tempo -> Hitbox -> Hitbox -> Velocidade-> Hitbox
cameraHitboxaux3 tempo hit jog (vx,vy)  | tb > ty = ((ta,tb-vy*tempo),(ba,bb-vy*tempo))
                                        | otherwise = hit
                            where   ((ta,tb),(ba,bb)) = hit
                                    ((tx,ty),(bx,by)) = jog
cameraHitboxaux4 :: Tempo -> Hitbox -> Hitbox -> Velocidade-> Hitbox
cameraHitboxaux4 tempo hit jog (vx,vy)  | bb < by = ((ta,tb+vy*tempo),(ba,bb+vy*tempo))
                                        | otherwise = hit
                            where   ((ta,tb),(ba,bb)) = hit
                                    ((tx,ty),(bx,by)) = jog
-- | Controlo final da camera
controlarCameraHitbox :: Int -> Mapa -> (Int,Int) -> Hitbox -> Hitbox -> Hitbox
controlarCameraHitbox control mapa (sizex,sizey) ((x,y),(z,w)) ((a,b),(c,d))
                                    | control == 0 = ((a,b),(c,d))
                                    | c > limDIRx = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((limDIRx-8,b),(limDIRx,d))
                                    | d > limDIRy = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((a,limDIRy-6),(c,limDIRy))
                                    | a < limESQx = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((limESQx,b),(limESQx+8,d))
                                    | b < limESQy = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((a,limESQy),(c,limESQy+6))
                                    | otherwise = ((a,b),(c,d))
                                    where ((limESQx,limESQy),(limDIRx,limDIRy)) = ((dimx+fromIntegral sizex/2/escalaGloss-4,dimy+(fromIntegral sizey/2)/escalaGloss-3),(dimw-fromIntegral sizex/2/escalaGloss+4,dimz-fromIntegral sizey/2/escalaGloss+3))
                                          ((dimx,dimy),(dimz,dimw))= getMapaDimensoes 1 mapa

--Camera control end 

--Boss AI START
-- | Controla o boss permitindo mais que um por mapa (nao aconselhavel)
bossMovimento :: Tempo -> Jogo -> Jogo
bossMovimento tempo jogo    | Boss `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> (ataque1Boss  x (jogador jogo)) ++ y) [] ( movimentaBoss (head(getcenterofhitbox 4 [(cameraControl jogo)])) tempo a) ++ b}
                            | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) Boss

-- | Controla as condiçoes de cada boss
movimentaBoss :: Posicao -> Tempo -> [Personagem] -> [Personagem]
movimentaBoss pos tempo bosses = map (\x -> x{aplicaDano = ((snd (aplicaDano boss) <= 30 && snd (aplicaDano boss) >= 25.5) || (snd (aplicaDano boss) <= 22 && snd (aplicaDano boss) >= 19) ,if snd (aplicaDano x)-tempo <= 15 then 30 else if snd (aplicaDano x) < 29.1 && snd (aplicaDano x) > 29 then 29 else
                                        if snd (aplicaDano x) < 28.1 && snd (aplicaDano x) > 28 then 28 else
                                        if snd (aplicaDano x) < 27.1 && snd (aplicaDano x) > 27 then 27 else
                                        if snd (aplicaDano x) < 22.1 && snd (aplicaDano x) > 22 then 22 else
                                        if snd (aplicaDano x) < 21.1 && snd (aplicaDano x) > 21 then 21 else
                                        if snd (aplicaDano x) < 20.1 && snd (aplicaDano x) > 20 then 20 else
                                        if snd (aplicaDano x) < 19.1 && snd (aplicaDano x) > 19 then 19 else
                                         snd (aplicaDano x)-tempo),posicao = (1.5,b)}) bosses
                    where   boss = head bosses
                            tboss = snd (aplicaDano boss)-tempo
                            (a,b) = pos

-- | Controla os aataques do boss
ataque1Boss :: Personagem -> Personagem -> [Personagem]
ataque1Boss boss jogador | tempo == 300 = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                         | tempo == 290 = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                         | tempo == 280 = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                         | tempo == 270 = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                         | tempo == 220 = [bolacanhao{velocidade = (11,-7),posicao = posicao boss},boss]
                         | tempo == 210 = [bolacanhao{velocidade = (11,-4),posicao = posicao boss},boss]
                         | tempo == 200 = [bolacanhao{velocidade = (13,-2),posicao = posicao boss},boss]
                         | tempo == 190 = [bolacanhao{velocidade = (15,0),posicao = posicao boss},boss]
                         | otherwise = [boss]
                        where   (condicao,anstestempo) = aplicaDano boss
                                tempo = floor (anstestempo *10)
                                (bx,by) = posicao boss
                                (jx,jy) = posicao jogador
                                (a,b) = (jx-bx,jy-by)
                                (c,d) = (a/sqrt (a^2 + b^2)*5,b/sqrt (a^2 + b^2)*5)
                                (t,y,u) = mira boss




-- | Controla os ataques de EyeEntidade e EyeBoss
ataqueDoBoss :: Tempo -> Personagem -> Personagem -> [Personagem]
ataqueDoBoss tempo jogador boss | not t = [boss]
                                | tboss == 8-10*tempo = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                                | tboss == 8-42*tempo && tipo boss /= EyeEntidade = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]

                            | otherwise = [boss]
                            where tboss = snd (aplicaDano boss)
                                  (bx,by) = posicao boss
                                  (jx,jy) = posicao jogador
                                  (a,b) = (jx-bx,jy-by)
                                  (c,d) = (a/sqrt (a^2 + b^2)*5,b/sqrt (a^2 + b^2)*5)
                                  (t,y,u) = mira boss
-- | Controla a fisica do CuspoDeFogo
movimentaCuspoJogo :: Tempo -> Jogo -> Jogo
movimentaCuspoJogo tempo jogo   | CuspoDeFogo `elem` map tipo (inimigos jogo) || BolaDeCanhao `elem` map tipo (inimigos jogo) = jogo {inimigos = movimentaCuspo (mapa jogo) tempo c ++ d}
                                | otherwise = jogo
                            where   (c,d) = onlyOneTipo (inimigos jogo) CuspoDeFogo

movimentaCuspo :: Mapa -> Tempo -> [Personagem] -> [Personagem]
movimentaCuspo mapa tempo cuspos | null cuspos = cuspos
                            | otherwise = foldl (\x y -> if sobreposicao ((a,b),(d,c)) (genHitbox y) then movimentaCuspoaux tempo y : x else x) [] cuspos
                            where ((a,b),(c,d)) = getMapaDimensoes 1 mapa

movimentaCuspoaux :: Tempo -> Personagem -> Personagem
movimentaCuspoaux tempo fogo = fogo {posicao = (fx+vx*tempo,fy+vy*tempo)}
                where   (fx,fy) = posicao fogo
                        (vx,vy) = velocidade fogo
--Boss AI END

--EYEBOSS START
-- | Controla o boss EyeBoss permitindo mais que um no mapa, mira e dispara contra o jogador
eyebossMovimento :: Tempo -> Jogo -> Jogo
eyebossMovimento tempo jogo     | EyeBoss `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> ataqueDoBoss tempo (jogador jogo) x ++ y) [] ( eyemovimentaBoss tempo a (jogador jogo)) ++ c ++ d}
                                | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) EyeBoss
                                    (c,d) = onlyOneTipo b CuspoDeFogo

-- | Controla as condiçoes cada EyeBoss
eyemovimentaBoss :: Tempo -> [Personagem] -> Personagem -> [Personagem]
eyemovimentaBoss tempo bosses jogador = map (\x -> x{aplicaDano = (snd (aplicaDano x) < 8 && snd (aplicaDano x) > 7,if snd (aplicaDano x)-tempo <= 0 then 8 else snd (aplicaDano x)-tempo),mira = (True,fst (posicao jogador)-fst (posicao x),snd (posicao jogador)-snd (posicao x))}) bosses



--EYEBOSS END

--EyeEntity START
-- | Controla os inimigos EyeEntity, mira e dispara contra o jogador
eyeentityMovimento :: Tempo -> Jogo -> Jogo
eyeentityMovimento tempo jogo     | EyeEntidade `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> ataqueDoBoss tempo (jogador jogo) x ++ y) [] ( eyemovimentaEntity tempo a (jogador jogo)) ++ c ++ d}
                                | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) EyeEntidade
                                    (c,d) = onlyOneTipo b CuspoDeFogo


eyemovimentaEntity :: Tempo -> [Personagem] -> Personagem -> [Personagem]
eyemovimentaEntity tempo bosses jogador = map (\x -> x{aplicaDano = (snd (aplicaDano x) < 8 && snd (aplicaDano x) > 7,if snd (aplicaDano x)-tempo <= 0 then 8 else snd (aplicaDano x)-tempo),mira = (distancia (posicao x) (posicao jogador) <= 8,fst (posicao jogador)-fst (posicao x),snd (posicao jogador)-snd (posicao x))}) bosses

--EyeEntity END

--Portas Start
-- | Controla as portas que só sao possiveis de abir com chave
portasFuncao :: Jogo -> Jogo
portasFuncao jogo = if not (temChave (jogador jogo)) then jogo {mapa = mapa jogo} else jogo {mapa = m, jogador = (jogador jogo) {temChave = m == mapa jogo && temChave (jogador jogo)}}
                    where m = podeabrirporta (mapa jogo) (jogador jogo)

podeabrirporta :: Mapa -> Personagem -> Mapa
podeabrirporta mapa player = acionarBlocoGeral mapa player Porta

acionarBlocoGeral :: Mapa -> Personagem -> Bloco -> Mapa
acionarBlocoGeral (Mapa a b c) jog bloco = Mapa a b (removerBloco (Mapa a b c) jog bloco)


removerBloco :: Mapa -> Personagem -> Bloco -> [[Bloco]]
removerBloco (Mapa a b c) jog bloco | not (any (sobreposicao ((p1-0.1,p2),(p3+0.1,p4))) (getMapColisions dimensaobloco [bloco] (dimensaobloco*0.5,dimensaobloco*0.5) (Mapa a b c))) = c
                                    | otherwise = removerBloco2 (dimensaobloco*0.5) c jog bloco
                                    where ((p1,p2),(p3,p4)) = genHitbox jog

removerBloco2 :: Double -> [[Bloco]] -> Personagem -> Bloco -> [[Bloco]]
removerBloco2 _ [] _ _ = []
removerBloco2 x l jog bloco | bloco `elem` head l = removerUmBloco x (dimensaobloco*0.5) (head l) jog bloco : removerBloco2 (x+dimensaobloco) (tail l) jog bloco
                            | otherwise = head l : removerBloco2 (x+dimensaobloco) (tail l) jog bloco

removerUmBloco :: Double -> Double -> [Bloco] -> Personagem -> Bloco -> [Bloco]
removerUmBloco _ _ [] _ _ = []
removerUmBloco y x l jog bloco  | sobreposicao ((p1-1,p2),(p3+1,p4)) ((p5,p6),(p7,p8)) && head l == bloco = Vazio : removerUmBloco y (x+dimensaobloco) (tail l) jog bloco
                                | otherwise = head l : removerUmBloco y (x+dimensaobloco) (tail l) jog bloco
                            where   ((p1,p2),(p3,p4)) = genHitbox jog
                                    ((p5,p6),(p7,p8)) = gethitboxbloco dimensaobloco (x,y)
--Portas End


--Canhao Start
-- | Controla os canhoes no jogo, entidade fixa, o seu ataque ocorre em tempo random e mira random também (pode existir mais que um)
canhaoMovimento :: Semente -> Tempo -> Jogo -> Jogo
canhaoMovimento semente tempo jogo | Canhao `elem` map tipo (inimigos jogo) || BolaDeCanhao `elem` map tipo (inimigos jogo) = jogo{inimigos = acaoCanhaoSpawnBola (acaoCanhao tempo (geraAleatorios semente (length a)) a) ++ acaoBolasDeCanhao (mapa jogo) tempo c ++ d}
                                   | otherwise = jogo
                           where (a,b) = onlyOneTipo (inimigos jogo) Canhao
                                 (c,d) = onlyOneTipo b BolaDeCanhao

acaoCanhaoSpawnBola :: [Personagem] -> [Personagem]
acaoCanhaoSpawnBola = foldl (\x y -> if fst (aplicaDano y) then bolacanhao{velocidade = doise (mira y),posicao = posicao y} : y : x else y : x) []
                        where doise :: (Bool,Double,Double) -> Velocidade
                              doise (a,b,c) = (b,c)

acaoCanhao :: Tempo -> [Int] -> [Personagem] -> [Personagem]
acaoCanhao _ _ [] = []
acaoCanhao _ [] _ = []
acaoCanhao tempo aleatorios canhoes = acaoCanhaoaux tempo (read (take 1 (show (abs (head aleatorios))))) (head canhoes) : acaoCanhao tempo (tail aleatorios) (tail canhoes)

acaoCanhaoaux :: Tempo -> Int -> Personagem -> Personagem
acaoCanhaoaux tempo aleatorio canhao = x{aplicaDano = (b == 3,if b-tempo <= 0 then 3+fromIntegral aleatorio/2 else if b > 2.5 && b < 3 then 3 else if b == 3 then 2.5 else b-tempo),mira = if b-tempo <= 0 then (True,bolax,bolay) else mira x}
                                where x = canhao
                                      (bolax,bolay) = (control,-10)
                                      (a,b) = aplicaDano x
                                      control = fromIntegral aleatorio-5
-- | Controla o percurso de cada bola de canhao
acaoBolasDeCanhao :: Mapa -> Tempo -> [Personagem] -> [Personagem]
acaoBolasDeCanhao mapa tempo lista = foldl (\x y -> if sobreposicao (genHitbox y) ((a,b),(d,c)) then y : x else x) [] (map (movimentoSemColisoes True tempo) lista)
                                        where ((a,b),(c,d)) = getMapaDimensoes 1 mapa


-- | Movimenta sem colisoes e o bool é se a gravidade o afeta ou nao (True sim,False nao)
movimentoSemColisoes :: Bool -> Tempo -> Personagem -> Personagem
movimentoSemColisoes controlo tempo ent | controlo = ent{posicao = (a+vx*tempo,b+vy*tempo),velocidade = (vx,vy+snd gravidade*tempo)}
                                        | otherwise = ent{posicao = (a+vx*tempo,b+vy*tempo),velocidade = (vx,vy)}
                                where (a,b) = posicao ent
                                      (vx,vy) = velocidade ent
--Canhao End

--ATIRADOR START
-- | Atirador entidade fixa que dispara foguetes é controlada aqui
atiradorMovimento :: Semente -> Tempo -> Jogo -> Jogo
atiradorMovimento semente tempo jogo | AtiradorBase `elem` map tipo (inimigos jogo) = jogo{inimigos = acaoAtiradorspawnFoguete (acaoAtirador tempo (geraAleatorios semente (length a)) a (jogador jogo)) ++ acaoAtiradorFoguetes (mapa jogo) tempo c ++ d}
                                     | otherwise = jogo
                           where (a,b) = onlyOneTipo (inimigos jogo) AtiradorBase
                                 (c,d) = onlyOneTipo b AtiradorFoguete

acaoAtiradorspawnFoguete  :: [Personagem] -> [Personagem]
acaoAtiradorspawnFoguete = foldl (\x y -> if fst (aplicaDano y) then atiradorfogueteent{velocidade = (if direcao y == Oeste then -5 else 5,0),posicao = (fst (posicao y),snd (posicao y)-0.5)} : y : x else y : x) []


acaoAtirador :: Tempo -> [Int] -> [Personagem] -> Personagem -> [Personagem]
acaoAtirador _ _ [] _ = []
acaoAtirador _ [] _ _ = []
acaoAtirador tempo aleatorios atiradores jogador = acaoAtiradoraux tempo (read (take 1 (show (abs (head aleatorios))))) (head atiradores) jogador : acaoAtirador tempo (tail aleatorios) (tail atiradores) jogador

acaoAtiradoraux :: Tempo -> Int -> Personagem -> Personagem -> Personagem
acaoAtiradoraux tempo aleatorio x jogador = x{aplicaDano = (b == 4,if b-tempo <= 0 then 4+fromIntegral aleatorio/2 else if b > 3.5 && b < 4 then 4 else if b == 4 then 3.5 else b-tempo),direcao = if fst (posicao jogador) > fst (posicao x) then Este else Oeste}
                                where (a,b) = aplicaDano x
                                      control = fromIntegral aleatorio-5
-- | Controla os foguetes do atirador
acaoAtiradorFoguetes :: Mapa -> Tempo -> [Personagem] -> [Personagem]
acaoAtiradorFoguetes mapa tempo lista = foldl (\x y -> if sobreposicao (genHitbox y) ((a,b),(d,c)) && not (any (sobreposicao (genHitbox y)) (getMapColisions dimensaobloco [Plataforma,Tunel,Alcapao,Porta] (dimensaobloco*0.5,dimensaobloco*0.5) mapa)) then y : x else x) [] (map (movimentoSemColisoes False tempo) lista)
                                        where ((a,b),(c,d)) = getMapaDimensoes 1 mapa

--ATIRADOR END

--CAOENEMY START
-- | Cao inimigo movel mas nao foge do espaço circular de 3 unidades, controla o cao nesta funçao
caoMovimenta :: Semente -> Tempo -> Jogo -> Jogo
caoMovimenta random tempo jogo | CaoEnemy `elem` map tipo (inimigos jogo) = jogo {inimigos = todosOsCaes (geraAleatorios random (length a)) (jogador jogo) tempo a ++ b}
                        | otherwise = jogo
                        where (a,b) = onlyOneTipo (inimigos jogo) CaoEnemy

todosOsCaes :: [Int] -> Personagem -> Tempo -> [Personagem] -> [Personagem]
todosOsCaes [] _ _ _ = []
todosOsCaes _ _ _ [] = []
todosOsCaes randoms jogador tempo lista = movimentoSemColisoes True tempo (caoAtaqueControlo tempo jogador $ caoIndividualDados (read (take 1 (show (abs (head randoms))))) tempo (head lista)) : todosOsCaes (tail randoms) jogador tempo (tail lista)

caoIndividualDados :: Int -> Tempo -> Personagem -> Personagem
caoIndividualDados control tempo cao = cao{aplicaDano = (b>2,if b-tempo < 0 then 6+(fromIntegral control/2) else b-tempo),mira = if not c then (True,fst (posicao cao),snd (posicao cao)) else mira cao}
                                where (a,b) = aplicaDano cao
                                      (c,d,e) = mira cao

caoAtaqueControlo :: Tempo -> Personagem -> Personagem -> Personagem
caoAtaqueControlo tempo jogador cao     | dano1 && distancia (mx,my) (bx,by) < 3 = cao{velocidade = (c,d)}
                                        | dano1 = cao{velocidade = (0,0)}
                                        | not dano1 && distancia (mx,my) (bx,by) > 1 = cao {velocidade = (cx,dy)}
                                        | otherwise = cao {velocidade = (0,0)}
                        where   (dano1,dano2) = aplicaDano cao
                                (bx,by) = posicao cao
                                (jx,jy) = posicao jogador
                                (a,b) = (jx-bx,jy-by)
                                (x,y) = (mx-bx,my-by)
                                (cx,dy) = (x/sqrt (x^2 + y^2)*5,y/sqrt (x^2 + y^2)*5)
                                (c,d) = (a/sqrt (a^2 + b^2)*5,b/sqrt (a^2 + b^2)*5)
                                (_,mx,my) = mira cao

--CAOENEMY END