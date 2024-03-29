module DrawLevel where


import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import GHC.Float (float2Double, double2Float, int2Float)
import Mapas
import Data.Maybe (fromJust)
import Utilities
import Data.Fixed (mod', Pico)
import Graphics.Gloss.Interface.Environment (getScreenSize)


-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss em relaçao á camera do jogo
posMapToGlossNivel :: Hitbox -> Posicao -> (Float,Float)
posMapToGlossNivel hit (x,y) = (a-4.5*d2f escalaGloss,b+2.5*d2f escalaGloss)
                                where   (a,b) =(d2f x*d2f escalaGloss - d2f (escalaGloss*fst jogador),- d2f y * d2f escalaGloss+ d2f (escalaGloss*snd jogador))
                                        jogador = head (getcenterofhitbox 1 [hit])






-- | Desenha o nivel desejado
drawLevel :: State -> Picture
drawLevel state = Pictures [
        drawEspinho jogo texEspinho,
        drawHitbox (cheats state) jogo (jogador jogo) (jogador jogo),
        drawPorta jogo texPorta,
        drawMap jogo texPlataforma,
        drawLadder jogo texEscada,
        drawColecs state texMoeda texmartelo2 texChave jogo,
        drawAlcapao jogo texAlcapao,
        drawTunel jogo texTunel,
        if fst $ aplicaDano (jogador jogo) then
            drawHammer jogo (playAnimAny 4 (time state) martelos) (jogador jogo)
        else
            blank,
        drawPlayer state (jogador jogo),
        drawEnemies state (texInimigo1,texInimigo2) texMacaco texBarril [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6] jogo,
        drawMorte jogo texMorte,drawCameracontrol (cheats state) texcamera jogo,
        drawNum ((currentScore state) + (pontos $ jogador jogo)) posPontuacao state, drawHud jogo state,
        if lostGame jogo == 5 then
            drawPause state
        else
            blank
    ]
    where texEscada = fromJust (lookup "escada" imagesPlatformTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesPlatformTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesPlatformTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo1 = fromJust (lookup "inimigo1" imagesTheme)
          texInimigo2 = fromJust (lookup "inimigo2" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesPlatformTheme)
          texPorta = fromJust (lookup "portaMario" imagesPlatformTheme)
          texMacaco = fromJust (lookup "macacoMalvado" imagesTheme)
          texBarril = fromJust (lookup "barril" imagesTheme)
          texMorte = fromJust (lookup "morreu" imagesTheme)
          texBoss1 = fromJust (lookup "boss1" imagesTheme)
          texBoss2 = fromJust (lookup "boss2" imagesTheme)
          texBoss3 = fromJust (lookup "boss3" imagesTheme)
          texBoss4 = fromJust (lookup "boss4" imagesTheme)
          texBoss5 = fromJust (lookup "boss5" imagesTheme)
          texBoss6 = fromJust (lookup "boss6" imagesTheme)
          texcamera = fromJust (lookup "cameraman" imagesTheme)
          texEspinho = fromJust (lookup "espinho" imagesTheme)
          martelos = [texmartelo1,texmartelo2]
          texmartelo1 = fromJust (lookup "martelo1" imagesTheme)
          texmartelo2 = fromJust (lookup "martelo2" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          imagesPlatformTheme = fromJust (lookup (platformTheme (options state)) (images state))
          (jogo, unlocked) = levels state !! currentLevel state
          posPontuacao = posMapToGloss state (1.4,0.5)
-- | Desenha a hitbox da camera
drawCameracontrol :: Bool ->Picture -> Jogo -> Picture
drawCameracontrol controlo pic jogo | controlo = color green $ rectangleWire (75*7) (75 * 5)
                                | otherwise = Pictures []

-- | Desenha o jogador no mapa e anima-o
drawPlayer :: State -> Personagem -> Picture
drawPlayer state jog = uncurry Translate (posMapToGlossNivel (cameraControl (fst (levels state !! currentLevel state))) (posicao jog)) $ Translate 0 7 $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
    scale (if direcao jog == Este then 2 else -2) 2 $ if animacaoJogo jogo > 0 || lostGame jogo == 4 || lostGame jogo == 0 then texMorreuMario else
    if snd (aplicaDano jog) > 0 && not (fst (aplicaDano jog)) then
        Rotate (360*escala) texMarioParado else
    if (fst (velocidade jog) == 4 || fst (velocidade jog) == (-4)) && snd (velocidade jog) >= 0 && snd (velocidade jog) <= 1 then
         playAnimAny (length marioCorrer+3) (time state) (if fst (aplicaDano jog) then mariocorrerMartelo else marioCorrer)
    else if (direcao jog == Norte || direcao jog == Sul) && emEscada jog then
        if (snd $ velocidade jog) /= 0 then playAnimAny 3 (time state) [texEscada1,texEscada2] else texEscada1
    else if snd (velocidade jog) == 0 then
         playAnimAny 2 (time state) [texandar3,texandar3,texandar3,texandar3,texandar3,texandar3,texolhos]
    else if fst (velocidade jog) == 0 then
        texMariocair
    else playAnimAny 5 (time state) saltarmario
    where texMariocair = fromJust (lookup "mariocair" imagesTheme)
          texMarioParado = fromJust (lookup "marioParado" imagesTheme)
          texMarioandar = fromJust (lookup "marioAndar1" imagesTheme)
          texMarioandar1 = fromJust (lookup "marioAndar2" imagesTheme)
          marioCorrer = [texandar1,texandar2,texandar3,texandar2]
          texandar1 = fromJust (lookup "marioandar1" imagesTheme)
          texandar2 = fromJust (lookup "marioandar2" imagesTheme)
          texandar3 = fromJust (lookup "marioandar3" imagesTheme)
          texolhos = fromJust (lookup "marioolhos" imagesTheme)
          texEscada1 = fromJust (lookup "escada1" imagesTheme)
          texEscada2 = fromJust (lookup "escada2" imagesTheme)
          saltarmario = [texsaltar1,texsaltar2]
          texsaltar1 = fromJust (lookup "mariosaltar1" imagesTheme)
          texsaltar2 = fromJust (lookup "mariosaltar2" imagesTheme)
          mariocorrerMartelo = [texMarioMartelo1,texMarioMartelo2,texMarioMartelo3,texMarioMartelo2]
          texMarioMartelo1 = fromJust (lookup "mariomartelo1" imagesTheme)
          texMarioMartelo2 = fromJust (lookup "mariomartelo2" imagesTheme)
          texMarioMartelo3 = fromJust (lookup "mariomartelo3" imagesTheme)
          texMorreuMario = fromJust (lookup "morreumario" imagesTheme)
          imagesTheme = fromJust (lookup (marioTheme (options state)) (images state))
          escala = realToFrac (snd (aplicaDano jog))
          (jogo, unlocked) = levels state !! currentLevel state

-- | Desenha todos os inimigos com ajuda de funçoes auxiliares
drawEnemies :: State ->  (Picture,Picture) -> Picture -> Picture -> [Picture] -> Jogo -> Picture
drawEnemies state inimigo texMacaco texBarril texBoss jogo = Pictures $ map (\x ->if tipo x == Fantasma then drawEnemy controlo jogo (playAnimAny 3 (time state) [fst inimigo, snd inimigo]) x (jogador jogo) else
                                                            if tipo x == MacacoMalvado then drawEnemy controlo jogo (if direcao x == Norte then texmacaco4 else (playAnimAny (length macacoAndar) (time state) macacoAndar)) x (jogador jogo) else if tipo x == Barril then drawEnemy controlo jogo texBarril x (jogador jogo) else
                                                            if tipo x == Boss then drawEnemy controlo jogo (if fst (aplicaDano x) then Scale 5.5 5.5 $ playAnimAny (length ataqueboss) (time state) ataqueboss else Scale 5.5 5.5 $ playAnimAny (length texBoss) (time state) texBoss) x (jogador jogo) else
                                                            if tipo x == CuspoDeFogo then drawEnemy controlo jogo (playAnimAny (length cuspobosstex) (time state) cuspobosstex) x (jogador jogo) else
                                                            drawMoreComplex state jogo controlo x)
                                                            (inimigos jogo)
                                                            where   ataqueboss = [texataque1,texataque2,texataque3,texataque4,texataque5,texataque6,texataque7,texataque8,texataque9,texataque10]
                                                                    texataque1 = fromJust (lookup "ataqueboss1" imagesTheme)
                                                                    texataque2 = fromJust (lookup "ataqueboss2" imagesTheme)
                                                                    texataque3 = fromJust (lookup "ataqueboss3" imagesTheme)
                                                                    texataque4 = fromJust (lookup "ataqueboss4" imagesTheme)
                                                                    texataque5 = fromJust (lookup "ataqueboss5" imagesTheme)
                                                                    texataque6 = fromJust (lookup "ataqueboss6" imagesTheme)
                                                                    texataque7 = fromJust (lookup "ataqueboss7" imagesTheme)
                                                                    texataque8 = fromJust (lookup "ataqueboss8" imagesTheme)
                                                                    texataque9 = fromJust (lookup "ataqueboss9" imagesTheme)
                                                                    texataque10 = fromJust (lookup "ataqueboss10" imagesTheme)
                                                                    cuspobosstex = [texCuspo1,texCuspo2,texCuspo3]
                                                                    texCuspo1 = fromJust (lookup "cuspo1" imagesTheme)
                                                                    texCuspo2 = fromJust (lookup "cuspo2" imagesTheme)
                                                                    texCuspo3 = fromJust (lookup "cuspo3" imagesTheme)
                                                                    texOlhobranco = fromJust (lookup "olhobranco" imagesTheme)
                                                                    texOlhoazul = fromJust (lookup "olhoazul" imagesTheme)
                                                                    macacoAndar = [texmacaco1,texmacaco2,texmacaco3,texmacaco2]
                                                                    texmacaco1 = fromJust (lookup "macacomalvado1" imagesTheme)
                                                                    texmacaco2 = fromJust (lookup "macacomalvado2" imagesTheme)
                                                                    texmacaco3 = fromJust (lookup "macacomalvado3" imagesTheme)
                                                                    texmacaco4 = fromJust (lookup "macacomalvado4" imagesTheme)
                                                                    imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
                                                                    escala = realToFrac (snd (aplicaDano jog))
                                                                    controlo = cheats state

-- | Funçao mais poderosa e versatil para criar texturas com mais que duas imagens (funçao mais recente onde menos inimigos a usam)
drawMoreComplex :: State -> Jogo -> Bool -> Personagem -> Picture
drawMoreComplex state jogo controlo inim    | tipo inim == EyeBoss = Pictures [starttranslate texOlhobranco,starttranslate $ Rotate (-atan2 (d2f mx) (d2f my) * 180 / pi) texOlhoazul, desenhahit]
                                            | tipo inim == EyeEntidade = Pictures [starttranslate $ Scale (0.25) (0.25) texOlhobranco, starttranslate $ Rotate (-atan2 (d2f mx) (d2f my) * 180 / pi) $ Scale (1/3) (1/3) texOlhoazul, desenhahit]
                                            | tipo inim == Canhao = Pictures [starttranslate $ Rotate (-atan2 (d2f mx) (d2f my) * 180 / pi) $ Scale (3) (3) canhaocano,starttranslate $ Scale (3) (3) canhaobase,  desenhahit]
                                            | tipo inim == BolaDeCanhao = Pictures [starttranslate $ scale 3 3 canhaobola, desenhahit]
                                            | tipo inim == AtiradorBase = Pictures [starttranslate $ scale (if direcao inim == Este then -3.2 else 3.2) 3.2 $ atiradorbase1, desenhahit]
                                            | tipo inim == AtiradorFoguete = Pictures [starttranslate $ scale (if fst (velocidade inim) > 0 then -2.1 else 2.1) 2.1 $ playAnimAny (length atiradorfogueteanim -1) (time state) atiradorfogueteanim, desenhahit]
                                            | tipo inim == CaoEnemy = Pictures [starttranslate $ Translate (d2f disx/14*1* d2f  escalaGloss) (d2f disy/14*1* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel1,starttranslate $ Translate (d2f disx/14*2* d2f  escalaGloss) (d2f disy/14*2* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel2,starttranslate $ Translate (d2f disx/14*3* d2f  escalaGloss) (d2f disy/14*3* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel1,
                                                                        starttranslate $ Translate (d2f disx/14*4* d2f  escalaGloss) (d2f disy/14*4* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel2,starttranslate $ Translate (d2f disx/14*5* d2f  escalaGloss) (d2f disy/14*5* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel1,starttranslate $ caoenemyanel2,
                                                                        starttranslate $ Translate (d2f disx/14*6* d2f  escalaGloss) (d2f disy/14*6* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel1,starttranslate $ Translate (d2f disx/14*7* d2f  escalaGloss) (d2f disy/14*7* d2f  escalaGloss) $ scale 1.8 1.8 caoenemyanel2,starttranslate $ scale 2.6 2.6 $ Rotate (-atan2 (d2f a) (d2f b) * 180 / pi) $ Rotate (-90)  $ playAnimAny (length caoanimacao + 2) (time state) caoanimacao, desenhahit]
                                            | otherwise = color red $ rectangleSolid 30 30
                                        where   texOlhobranco = fromJust (lookup "olhobranco" imagesTheme)
                                                texOlhoazul = fromJust (lookup "olhoazul" imagesTheme)
                                                canhaocano = fromJust (lookup "canhaocano" imagesTheme)
                                                canhaobase = fromJust (lookup "canhaobase" imagesTheme)
                                                canhaobola = fromJust (lookup "canhaobola" imagesTheme)
                                                atiradorfogueteanim = [atiradorfoguete1,atiradorfoguete2,atiradorfoguete3,atiradorfoguete4]
                                                atiradorfoguete1 = fromJust (lookup "atiradorfoguete1" imagesTheme)
                                                atiradorfoguete2 = fromJust (lookup "atiradorfoguete2" imagesTheme)
                                                atiradorfoguete3 = fromJust (lookup "atiradorfoguete3" imagesTheme)
                                                atiradorfoguete4 = fromJust (lookup "atiradorfoguete4" imagesTheme)
                                                atiradorbase1 = fromJust (lookup "atiradorbase1" imagesTheme)
                                                caoanimacao = [caoenemy1,caoenemy2]
                                                caoenemy1 = fromJust (lookup "caoenemy1" imagesTheme)
                                                caoenemy2 = fromJust (lookup "caoenemy2" imagesTheme)
                                                caoenemyanel1 = if (currentMenu state /= LevelEditor) then fromJust (lookup "caoenemyanel1" imagesTheme) else blank
                                                caoenemyanel2 = if (currentMenu state /= LevelEditor) then fromJust (lookup "caoenemyanel2" imagesTheme) else blank
                                                imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
                                                starttranslate x = Translate (fst $ posMapToGlossNivel (cameraControl jogo) (posicao inim)) (0.3+snd (posMapToGlossNivel (cameraControl jogo) (posicao inim))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) x
                                                desenhahit = drawHitbox controlo jogo (jogador jogo) inim
                                                (_,mx,my) = mira inim
                                                (bx,by) = posicao inim
                                                (jx,jy) = posicao (jogador jogo)
                                                (a,b) = (jx-bx,jy-by)
                                                (disx,disy) = (-(bx-mx),-(my-by))
-- | Funçao que desenha inimigos menos poderosa mas facil de usar em inimigos simples
drawEnemy :: Bool -> Jogo -> Picture -> Personagem -> Personagem -> Picture
drawEnemy controlo jogo tex inim jogador = Pictures [Translate (fst $ posMapToGlossNivel (cameraControl jogo) (posicao inim)) (0.3+snd (posMapToGlossNivel (cameraControl jogo) (posicao inim))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
                                if tipo inim == Barril then Rotate (fromInteger (floor (snd (posicao inim)))*90) $ Scale (if fst (velocidade inim) > 0 then 0.7 else -0.7) 0.7 tex else
                                if tipo inim == Boss then Scale (if fst (posicao jogador) > fst (posicao inim) then -2.2 else 2.2) 2.2 tex else
                                if tipo inim == CuspoDeFogo then Scale 2 2 $ Rotate (-atan2 (d2f vx) (d2f vy) * 180 / pi) tex else
                                if tipo inim == Fantasma then Scale (if fst (velocidade inim) > 0 then 2.3 else -2.3) 2.3 tex else
                                if tipo inim == MacacoMalvado then scale (if direcao inim == Este then -1.7 else 1.7 ) 1.7 tex else -- se a direcao for norte está parado
                                Scale (if fst (velocidade inim) > 0 then 1 else -1) 1 tex
                                , drawHitbox controlo jogo jogador inim]
                                where (vx,vy) = velocidade inim
-- | Desenha a hitbox da Personagem desejada
drawHitbox :: Bool -> Jogo -> Personagem -> Personagem -> Picture
drawHitbox controlo jogo jogador inm    | controlo = (Color green $ uncurry Translate (posMapToGlossNivel (cameraControl jogo) (posicao inm)) $ rectangleWire tx ty)
                                        | otherwise = Pictures []
    where tx = fst (snd $ aux (genHitbox inm)) - fst (fst $ aux (genHitbox inm))
          ty = snd (snd $ aux (genHitbox inm)) - snd (fst $ aux (genHitbox inm))
          aux :: Hitbox -> ((Float,Float),(Float,Float))
          aux (p1,p2) = (posMapToGlossNivel (cameraControl jogo) p1, posMapToGlossNivel (cameraControl jogo) p2)
-- | Desenha os colecionaveis desejados
drawColecs :: State -> Picture -> Picture -> Picture -> Jogo -> Picture
drawColecs state moeda martelo chave jogo = Pictures $ map (\(colec,pos) -> if colec == Moeda then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) (Scale 2 2 (playAnimAny (length moedaanim) (time state) moedaanim)) else
                                                     if colec == Martelo then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale 2 2 $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) martelo else
                                                     if colec == Chave then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) chave else
                                                     if colec == Estrela then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) (playAnimAny (length estrelaAnim -3) (time state) estrelaAnim) else
                                                     if colec == CogumeloVida then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ scale 2 2 cogumelovida1 else
                                                     uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) martelo)
                                                     (colecionaveis jogo)
                                                     where  estrelaAnim = [estrela1,estrela2,estrela3,estrela4,estrela5,estrela6,estrela7,estrela8,estrela9,estrela10,estrela11,estrela12]
                                                            estrela1 = fromJust (lookup "estrela1" imagesTheme)
                                                            estrela2 = fromJust (lookup "estrela2" imagesTheme)
                                                            estrela3 = fromJust (lookup "estrela3" imagesTheme)
                                                            estrela4 = fromJust (lookup "estrela4" imagesTheme)
                                                            estrela5 = fromJust (lookup "estrela5" imagesTheme)
                                                            estrela6 = fromJust (lookup "estrela6" imagesTheme)
                                                            estrela7 = fromJust (lookup "estrela7" imagesTheme)
                                                            estrela8 = fromJust (lookup "estrela8" imagesTheme)
                                                            estrela9 = fromJust (lookup "estrela9" imagesTheme)
                                                            estrela10 = fromJust (lookup "estrela10" imagesTheme)
                                                            estrela11 = fromJust (lookup "estrela11" imagesTheme)
                                                            estrela12 = fromJust (lookup "estrela12" imagesTheme)
                                                            moedaanim = [moeda1,moeda2,moeda3,moeda2]
                                                            moeda1 = fromJust (lookup "moeda1" imagesTheme)
                                                            moeda2 = fromJust (lookup "moeda2" imagesTheme)
                                                            moeda3 = fromJust (lookup "moeda3" imagesTheme)
                                                            cogumelovida1 = fromJust (lookup "cogumelovida1" imagesTheme)

                                                            imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
-- | Desenha o martelo
drawHammer :: Jogo -> Picture -> Personagem -> Picture
drawHammer jogo tex jog = Translate (pox + (if direcao jog == Este then d2f ((tamx/1.5)*escalaGloss) else -d2f ((tamx/1.5)*escalaGloss))) (poy + 0.15*d2f escalaGloss) $ Scale (if dir == Este then 1 else -1) 1 $ (if dir == Norte || dir == Sul then scale 0 0 else scale 2 2) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) tex
                        where   (pox,poy) = posMapToGlossNivel (cameraControl jogo) (posicao jog)
                                (tamx,tamy) = tamanho jog
                                dir = direcao jog


-- | Desenha o Hud do jogo
drawHud :: Jogo -> State -> Picture
drawHud jogo state = Pictures $ (genPics posmaphearts (vida $ jogador jogo) ++ [
        Translate posx posy $ scale 2.5 2.5 texMarioface,
        Translate posmoedax posmoeday $ scale 2.5 2.5 $ playAnimAny (length moedaanim) (time state) moedaanim,
        Translate igualx igualy $ scale 2.5 2.5 igual
    ])
    where genPics :: (Float, Float) -> Int -> [Picture]
          genPics (px,py) n
            | n == 0 = []
            | otherwise = Translate (px + (fromIntegral n*40)) py (scale 0.8 0.8 texHearts) : genPics (px,py) (n-1)
          posmaphearts = posMapToGloss state (0.8,1.5)
          (posx,posy) = posMapToGloss state (0.5,1.5)
          (posmoedax,posmoeday) = posMapToGloss state (0.3,0.5)
          (igualx,igualy) = posMapToGloss state (0.77,0.5)
          --Texturas
          texHearts = fromJust (lookup "hearts" imagesTheme)
          texMarioface = fromJust (lookup "marioface" imagesTheme)
          moedaanim = [moeda1,moeda2,moeda3,moeda2]
          moeda1 = fromJust (lookup "moeda1" imagesTheme)
          moeda2 = fromJust (lookup "moeda2" imagesTheme)
          moeda3 = fromJust (lookup "moeda3" imagesTheme)
          igual = fromJust (lookup "igual" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))



-- | Animaçao da morte do jogador
drawMorte :: Jogo -> Picture -> Picture
drawMorte jogo img = uncurry Translate (posMapToGlossNivel (cameraControl jogo) (posicao (jogador jogo))) $ if animacaoJogo jogo > 0 || lostGame jogo == 4 || lostGame jogo == 0 then scale (20*escala) (20*escala) img else scale 0 0 img
                    where escala = realToFrac (animacaoJogo jogo)*2+0.4

-- | Desenha o mapa plataformas
drawMap :: Jogo -> Picture -> Picture
drawMap jogo img = Pictures $ map (\pos -> Color white $ uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) 

-- | Desenha as escadas do mapa
drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Escada] (1*0.5,1*0.5) (mapa jogo)))

-- | Desenha os Tuneis
drawTunel :: Jogo -> Picture -> Picture
drawTunel jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Tunel] (1*0.5,1*0.5) (mapa jogo)))

-- | Desenha as Portas
drawPorta :: Jogo -> Picture -> Picture
drawPorta jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Porta] (1*0.5,1*0.5) (mapa jogo)))

-- | Desenha os Espinhos
drawEspinho :: Jogo -> Picture -> Picture
drawEspinho jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Espinho] (1*0.5,1*0.5) (mapa jogo)))

 -- | Desenha os alçapoes
drawAlcapao :: Jogo -> Picture -> Picture
drawAlcapao jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Alcapao] (1*0.5,1*0.5) (mapa jogo)))



-- | Anima só com duas imagens
playAnim :: Float -> [Picture] -> Picture
playAnim time texs
    | (time `mod'` (1/n)) < 1/(n*2) = head texs
    | otherwise = texs !! 1
    where n = 6 -- Animation speed

-- | Anima qualquer lista de imagens com controlo de tempo
playAnimAny :: Int -> Float -> [Picture] -> Picture
playAnimAny x time texs = texs !! frame
  where
    n = fromIntegral x -- Número de frames
    frame = floor (time * n) `mod` length texs

-- | Inetracao do jogador
eventHandlerInGame :: Event -> Jogo -> Jogo
eventHandlerInGame (EventKey (Char 'd') Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarDireita) jogo
eventHandlerInGame (EventKey (Char 'd') Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (Char 'a') Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarEsquerda) jogo
eventHandlerInGame (EventKey (Char 'a') Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (Char 'w') Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Subir) jogo
eventHandlerInGame (EventKey (Char 'w') Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (Char 's') Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Descer) jogo
eventHandlerInGame (EventKey (Char 's') Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Saltar) jogo
eventHandlerInGame e jogo = jogo

-- | Desenha a Pausa
drawPause :: State -> Picture
drawPause state = Translate 0 70 $ Pictures [
        Translate 0 20 $ scale (4 * d2f escalaGloss/50) (4 * d2f escalaGloss/50) pauseTex,
        drawButton (images state) "botaostart" (selectedButton (menuState state), 0) (pressingButton (menuState state)),
        drawButton (images state) "botaoRestart" (selectedButton (menuState state), 1) (pressingButton (menuState state)),
        drawButton (images state) "botaoQuit" (selectedButton (menuState state), 2) (pressingButton (menuState state))
    ]
    where imagesTheme = fromJust (lookup Default (images state))
          pauseTex = fromJust (lookup "pauseScreen" imagesTheme)

