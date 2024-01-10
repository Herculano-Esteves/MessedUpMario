module Extras where

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Mapas
import Utilities

extrasFuncao :: Semente -> Tempo -> State -> State
extrasFuncao random tempo state = state{levels = replace (levels state) (currentLevel state,(movimentaExtras tempo random (screenSize state) jogo, unlocked))}
                            where (jogo, unlocked) = levels state !! currentLevel state



movimentaExtras :: Tempo -> Semente -> (Int,Int) -> Jogo -> Jogo
movimentaExtras dtime random screen jogo    | lostGame jogo == 5 = jogo
                                            | lostGame jogo == 2 = perdeVidaJogadorEnd dtime jogo
                                            | otherwise = cameraHitbox dtime $ bossMovimento dtime jogo





--Contrlo de Camera
cameraHitbox :: Tempo -> Jogo -> Jogo
cameraHitbox tempo jogo = jogo {cameraControl = cameraHitboxaux (mapa jogo) tempo (jogador jogo) (cameraControl jogo) }

cameraHitboxaux :: Mapa -> Tempo -> Personagem -> Hitbox -> Hitbox
cameraHitboxaux mapa tempo jog hit = controlarCameraHitbox $ cameraHitboxaux4 tempo (cameraHitboxaux3 tempo (cameraHitboxaux2 tempo (cameraHitboxaux1 tempo hit ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)
                     where  ((ta,tb),(ba,bb)) = hit
                            ((tx,ty),(bx,by)) = ((z-1,w-1),(z+1,w+1))
                            (z,w) = posicao jog
                            (vx,vy) = (if abs (fst (velocidade jog)) == 0 then 1 else abs (fst (velocidade jog)),if abs (snd (velocidade jog)) == 0 then 1 else abs (snd (velocidade jog)))
                            ((a,b),(c,d)) = getMapaDimensoes 1 mapa
-- Nome XD
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

controlarCameraHitbox :: Hitbox -> Hitbox
controlarCameraHitbox ((a,b),(c,d)) | a < 6.65 = controlarCameraHitbox ((6.65,b),(6.65+8,d))
                                    | b < 3 = controlarCameraHitbox ((a,3),(c,3+6))
                                    | otherwise = ((a,b),(c,d))


--Camera control end 

--Boss AI START
bossMovimento :: Tempo -> Jogo -> Jogo
bossMovimento tempo jogo    | Boss `elem` map tipo (inimigos jogo) = jogo {inimigos = ataqueDoBoss tempo (jogador jogo) ( movimentaBoss tempo a) ++ movimentaCuspo (mapa jogo) tempo c ++ d}
                            | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) Boss
                                    (c,d) = onlyOneTipo b CuspoDeFogo

--Sabendo que so pode existir um boss
movimentaBoss :: Tempo -> [Personagem] -> Personagem
movimentaBoss tempo bosses = boss {aplicaDano = (snd (aplicaDano boss) < 8 && snd (aplicaDano boss) > 7,if tboss <= 0 then 8 else tboss)}
                    where   boss = head bosses
                            tboss = snd (aplicaDano boss)-tempo

ataqueDoBoss :: Tempo -> Personagem -> Personagem -> [Personagem]
ataqueDoBoss tempo jogador boss   | tboss == 8-10*tempo = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                            | tboss == 8-42*tempo = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]

                            | otherwise = [boss]
                            where tboss = snd (aplicaDano boss)
                                  (bx,by) = posicao boss
                                  (jx,jy) = posicao jogador
                                  (a,b) = (jx-bx,jy-by)
                                  (c,d) = (a/sqrt (a^2 + b^2)*5,b/sqrt (a^2 + b^2)*5)

movimentaCuspo :: Mapa -> Tempo -> [Personagem] -> [Personagem]
movimentaCuspo mapa tempo cuspos | null cuspos = cuspos
                            | otherwise = foldl (\x y -> if sobreposicao ((a,b),(d,c)) (genHitbox y) then movimentaCuspoaux tempo y : x else x) [] cuspos
                            where ((a,b),(c,d)) = getMapaDimensoes 1 mapa

movimentaCuspoaux :: Tempo -> Personagem -> Personagem
movimentaCuspoaux tempo fogo = fogo {posicao = (fx+vx*tempo,fy+vy*tempo)}
                where   (fx,fy) = posicao fogo
                        (vx,vy) = velocidade fogo
--Boss AI END