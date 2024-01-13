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
                                                | otherwise = movimentaCuspoJogo dtime $ eyeentityMovimento dtime $ eyebossMovimento dtime $ cheatModeAtualiza (cheats state) $ cameraHitbox screen dtime $ bossMovimento dtime jogo

--Cheats START
cheatModeAtualiza :: Bool -> Jogo -> Jogo
cheatModeAtualiza condicao jogo = jogo {cheatsjogo= condicao}

--Cheats END




--Contrlo de Camera
cameraHitbox :: (Int,Int) -> Tempo -> Jogo -> Jogo
cameraHitbox (a,b) tempo jogo = jogo {cameraControl = cameraHitboxaux (a,b) (mapa jogo) tempo (jogador jogo) (cameraControl jogo) }

cameraHitboxaux :: (Int,Int) -> Mapa -> Tempo -> Personagem -> Hitbox -> Hitbox
cameraHitboxaux (h,t) mapa tempo jog hit = controlarCameraHitbox 4 mapa (h,t) ((a,b),(c,d)) $ cameraHitboxaux4 tempo (cameraHitboxaux3 tempo (cameraHitboxaux2 tempo (cameraHitboxaux1 tempo hit ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)) ((tx,ty),(bx,by)) (vx,vy)
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

controlarCameraHitbox :: Int -> Mapa -> (Int,Int) -> Hitbox -> Hitbox -> Hitbox
controlarCameraHitbox control mapa (sizex,sizey) ((x,y),(z,w)) ((a,b),(c,d))
                                    | control == 0 = ((a,b),(c,d))
                                    | c > limDIRx = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((limDIRx-8,b),(limDIRx,d))
                                    | d > limDIRy = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((a,limDIRy-6),(c,limDIRy))
                                    | a < limESQx = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((limESQx,b),(limESQx+8,d))
                                    | b < limESQy = controlarCameraHitbox (control-1) mapa (sizex,sizey) ((x,y),(z,w)) ((a,limESQy),(c,limESQy+6))
                                    | otherwise = ((a,b),(c,d))
                                    where ((limESQx,limESQy),(limDIRx,limDIRy)) = ((dimx+((fromIntegral sizex/2)/escalaGloss)-4,(dimy+((fromIntegral sizey/2)/escalaGloss))-3),(dimw-((fromIntegral sizex/2)/escalaGloss)+4,dimz-((fromIntegral sizey/2)/escalaGloss)+3))
                                          ((dimx,dimy),(dimz,dimw))= getMapaDimensoes 1 mapa



--Camera control end 

--Boss AI START
bossMovimento :: Tempo -> Jogo -> Jogo
bossMovimento tempo jogo    | Boss `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> ataqueDoBoss tempo (jogador jogo) x ++ y) [] ( movimentaBoss tempo a) ++ c ++ d}
                            | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) Boss
                                    (c,d) = onlyOneTipo b CuspoDeFogo

--Sabendo que so pode existir um boss
movimentaBoss :: Tempo -> [Personagem] -> [Personagem]
movimentaBoss tempo bosses = map (\x -> x{aplicaDano = (snd (aplicaDano boss) < 8 && snd (aplicaDano boss) > 7,if (snd (aplicaDano x)-tempo) <= 0 then 8 else snd (aplicaDano x)-tempo)}) bosses
                    where   boss = head bosses
                            tboss = snd (aplicaDano boss)-tempo

ataqueDoBoss :: Tempo -> Personagem -> Personagem -> [Personagem]
ataqueDoBoss tempo jogador boss | mira boss == (-100,-100) = [boss]
                                | tboss == 8-10*tempo = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]
                                | tboss == 8-42*tempo && tipo boss /= EyeEntidade = [cuspopersonagem{posicao = (bx,by),velocidade = (c,d)},boss]

                            | otherwise = [boss]
                            where tboss = snd (aplicaDano boss)
                                  (bx,by) = posicao boss
                                  (jx,jy) = posicao jogador
                                  (a,b) = (jx-bx,jy-by)
                                  (c,d) = (a/sqrt (a^2 + b^2)*5,b/sqrt (a^2 + b^2)*5)

movimentaCuspoJogo :: Tempo -> Jogo -> Jogo
movimentaCuspoJogo tempo jogo   | CuspoDeFogo `elem` map tipo (inimigos jogo) = jogo {inimigos = movimentaCuspo (mapa jogo) tempo c ++ d}
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

eyebossMovimento :: Tempo -> Jogo -> Jogo
eyebossMovimento tempo jogo     | EyeBoss `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> ataqueDoBoss tempo (jogador jogo) x ++ y) [] ( eyemovimentaBoss tempo a (jogador jogo)) ++ c ++ d}
                                | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) EyeBoss
                                    (c,d) = onlyOneTipo b CuspoDeFogo


eyemovimentaBoss :: Tempo -> [Personagem] -> Personagem -> [Personagem]
eyemovimentaBoss tempo bosses jogador = map (\x -> x{aplicaDano = (snd (aplicaDano x) < 8 && snd (aplicaDano x) > 7,if (snd (aplicaDano x)-tempo) <= 0 then 8 else snd (aplicaDano x)-tempo),mira = (fst(posicao jogador)-fst(posicao x),snd(posicao jogador)-snd(posicao x))}) bosses
                   
                            

--EYEBOSS END

--EyeEntity START
eyeentityMovimento :: Tempo -> Jogo -> Jogo
eyeentityMovimento tempo jogo     | EyeEntidade `elem` map tipo (inimigos jogo) = jogo {inimigos = foldl (\y x -> ataqueDoBoss tempo (jogador jogo) x ++ y) [] ( eyemovimentaEntity tempo a (jogador jogo)) ++ c ++ d}
                                | otherwise = jogo
                            where   (a,b) = onlyOneTipo (inimigos jogo) EyeEntidade
                                    (c,d) = onlyOneTipo b CuspoDeFogo


eyemovimentaEntity :: Tempo -> [Personagem] -> Personagem -> [Personagem]
eyemovimentaEntity tempo bosses jogador = map (\x -> x{aplicaDano = (snd (aplicaDano x) < 8 && snd (aplicaDano x) > 7,if (snd (aplicaDano x)-tempo) <= 0 then 8 else snd (aplicaDano x)-tempo),mira = if distancia (posicao x) (posicao jogador) > 12 then (-100,-100) else (fst(posicao jogador)-fst(posicao x),snd(posicao jogador)-snd(posicao x))}) bosses
                   
--EyeEntity END