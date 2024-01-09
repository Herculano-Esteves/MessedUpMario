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
import Data.Fixed (mod')
import Graphics.Gloss.Interface.Environment (getScreenSize)


-- | Devolve o tamanho da janela apropriado para um determinado mapa inicial e uma escala dos blocos
-- sizeWin :: (Int, Int)
-- sizeWin = (round $ snd (snd (getMapaDimensoes escalaGloss (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)))), round $ fst (snd (getMapaDimensoes escalaGloss (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)))))

-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss
-- posMapToGloss :: Posicao -> (Float,Float)
-- posMapToGloss (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)

posMapToGlossNivel :: Hitbox -> Posicao -> (Float,Float)
posMapToGlossNivel hit (x,y) = (a-4.5*d2f escalaGloss,b+2.5*d2f escalaGloss)
                                where   (a,b) =(d2f x*d2f escalaGloss - d2f (escalaGloss*fst jogador),- d2f y * d2f escalaGloss+ d2f (escalaGloss*snd jogador))
                                        jogador = head (getcenterofhitbox 1 [hit])






d2f = double2Float
f2d = float2Double

drawLevel :: State -> Picture
drawLevel state = Pictures [drawEspinho jogo texEspinho,drawHitbox (cheats state) jogo (jogador jogo) (jogador jogo), drawHud jogo texPlataforma, drawBackground jogo texPlataforma,drawLadder jogo texEscada, drawPorta jogo texPorta, drawMap jogo texPlataforma, drawColecs state texMoeda texMartelo texChave jogo, drawAlcapao jogo texAlcapao, drawTunel jogo texTunel,
                if fst $ aplicaDano (jogador jogo) then drawHammer jogo texMartelo (jogador jogo) else blank, drawPlayer state (jogador jogo),drawEnemies state (texCuspo1,texCuspo2) (texInimigo1,texInimigo2) texMacaco texBarril [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6] jogo, drawMorte jogo texMorte,drawCameracontrol (cheats state) texcamera jogo, drawNum (pontos $ jogador jogo) (700,400) state]
    where texEscada = fromJust (lookup "escada" imagesTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo1 = fromJust (lookup "inimigo1" imagesTheme)
          texInimigo2 = fromJust (lookup "inimigo2" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texMartelo = fromJust (lookup "martelo" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesTheme)
          texPorta = fromJust (lookup "portaMario" imagesTheme)
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
          texCuspo1 = fromJust (lookup "cuspo1" imagesTheme)
          texCuspo2 = fromJust (lookup "cuspo2" imagesTheme)
          texEspinho = fromJust (lookup "espinho" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          (jogo, unlocked) = levels state !! currentLevel state

drawCameracontrol :: Bool ->Picture -> Jogo -> Picture
drawCameracontrol controlo pic jogo | controlo = color green $ rectangleWire (75*7) (75 * 5)
                                | otherwise = Pictures []

-- ? Set a scale for drawng according to the size of the wind
-- TODO: Check if the code "$ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $" is actually working properly
drawPlayer :: State -> Personagem -> Picture
drawPlayer state jog = uncurry Translate (posMapToGlossNivel (cameraControl (fst (levels state !! currentLevel state))) (posicao jog)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
    scale (if direcao jog == Este then 1 else -1) 1 $
    if snd (aplicaDano jog) > 0 && not (fst (aplicaDano jog)) then Rotate (360*escala) texMarioParado else
    if (fst (velocidade jog) == 4 || fst (velocidade jog) == (-4)) && snd (velocidade jog) >= 0 && snd (velocidade jog) <= 1 then
        playAnim (time state) [texMarioandar, texMarioandar1]
    else if snd (velocidade jog) == 0 then
        texMarioParado
    else (if fst (velocidade jog) == 0 then texMariocair else texMariosaltar)
    where texMariocair = fromJust (lookup "mariocair" imagesTheme)
          texMarioParado = fromJust (lookup "marioParado" imagesTheme)
          texMarioandar = fromJust (lookup "marioAndar1" imagesTheme)
          texMarioandar1 = fromJust (lookup "marioAndar2" imagesTheme)
          texMariosaltar = fromJust (lookup "mariosaltar" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          escala = realToFrac (snd (aplicaDano jog))

-- (if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then picandar else
drawEnemies :: State -> (Picture,Picture) ->  (Picture,Picture) -> Picture -> Picture -> [Picture] -> Jogo -> Picture
drawEnemies state cuspo inimigo texMacaco texBarril texBoss jogo = Pictures $ map (\x ->if tipo x == Fantasma then drawEnemy controlo jogo (playAnimAny 3 (time state) [fst inimigo, snd inimigo]) x (jogador jogo) else
                                                            if tipo x == MacacoMalvado then drawEnemy controlo jogo texMacaco x (jogador jogo) else if tipo x == Barril then drawEnemy controlo jogo texBarril x (jogador jogo) else
                                                            if tipo x == Boss then drawEnemy controlo jogo (if fst (aplicaDano x) then playAnimAny (length ataqueboss) (time state) ataqueboss else playAnimAny (length texBoss) (time state) texBoss) x (jogador jogo) else
                                                            if tipo x == CuspoDeFogo then drawEnemy controlo jogo (if even (floor (fst (posicao x)) + floor (snd (posicao x))) then fst cuspo else snd cuspo) x (jogador jogo) else drawEnemy controlo jogo texBarril x (jogador jogo))
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
                                                                    imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
                                                                    escala = realToFrac (snd (aplicaDano jog))
                                                                    controlo = cheats state



drawEnemy :: Bool -> Jogo -> Picture -> Personagem -> Personagem -> Picture
drawEnemy controlo jogo tex inim jogador = Pictures [Translate (fst $ posMapToGlossNivel (cameraControl jogo) (posicao inim)) (0.3+snd (posMapToGlossNivel (cameraControl jogo) (posicao inim))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
                                if tipo inim == Barril then Rotate (fromInteger (floor (snd (posicao inim)))*90) $ Scale (if fst (velocidade inim) > 0 then 1 else -1) 1 tex else
                                if tipo inim == Boss then Scale (if fst (posicao jogador) > fst (posicao inim) then -2.2 else 2.2) 2.2 tex else
                                if tipo inim == CuspoDeFogo then Rotate (-atan2 (d2f vx) (d2f vy) * 180 / pi) tex else
                                if tipo inim == Fantasma then Scale (if fst (velocidade inim) > 0 then 1.2 else -1.2) 1.2 tex else
                                Scale (if fst (velocidade inim) > 0 then 1 else -1) 1 tex
                                , drawHitbox controlo jogo jogador inim]
                                where (vx,vy) = velocidade inim

drawHitbox :: Bool -> Jogo -> Personagem -> Personagem -> Picture
drawHitbox controlo jogo jogador inm    | controlo = (Color green $ uncurry Translate (posMapToGlossNivel (cameraControl jogo) (posicao inm)) $ rectangleWire tx ty)
                                        | otherwise = Pictures []
    where tx = fst (snd $ aux (genHitbox inm)) - fst (fst $ aux (genHitbox inm))
          ty = snd (snd $ aux (genHitbox inm)) - snd (fst $ aux (genHitbox inm))
          aux :: Hitbox -> ((Float,Float),(Float,Float))
          aux (p1,p2) = (posMapToGlossNivel (cameraControl jogo) p1, posMapToGlossNivel (cameraControl jogo) p2)

drawColecs :: State -> Picture -> Picture -> Picture -> Jogo -> Picture
drawColecs state moeda martelo chave jogo = Pictures $ map (\(colec,pos) -> if colec == Moeda then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) (Scale 0.7 0.7 moeda) else
                                                     if colec == Martelo then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) martelo else
                                                     if colec == Chave then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) chave else
                                                     if colec == Estrela then uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) (playAnimAny (length estrelaAnim -3) (time state) estrelaAnim) else
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
                                                            imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))

drawHammer :: Jogo -> Picture -> Personagem -> Picture
drawHammer jogo tex jog = Translate (fst (posMapToGlossNivel (cameraControl jogo) (posicao jog)) + (if direcao jog == Este then d2f escalaGloss else d2f (-escalaGloss))) (snd (posMapToGlossNivel (cameraControl jogo) (posicao jog))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) tex

-- TODO: Maybe we should make the get center of hitbox not receive a scale to avoid having to set it to 1
drawMap :: Jogo -> Picture -> Picture
drawMap jogo img = Pictures $ map (\pos -> Color white $ uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) -- ++
    --map (\pos -> Color white $ uncurry Translate (posMapToGloss pos) $ Color green $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
{-
drawBackground :: Jogo -> Picture -> Picture
drawBackground jogo img = Pictures $ foldl (\guarda (a,b)-> map (\(xx,yy) -> uncurry Translate (posMapToGlossNivel (jogador jogo) (xx+a,yy+b)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma,Vazio,Alcapao,Tunel,Porta,Escada] (1*0.5,1*0.5) (mapa jogo))) ++ guarda) [] [(x,y),(x,0),(-x,0),(x,-y),(0,y),(0,-y),(-x,y),(-x,-y)]
                            where (x,y) = snd (getMapaDimensoes escalaGloss (mapa jogo))
-}
drawBackground :: Jogo -> Picture -> Picture
drawBackground jogo tex = pictures []

drawHud :: Jogo -> Picture -> Picture
drawHud jogo tex1 = pictures []




drawMorte :: Jogo -> Picture -> Picture
drawMorte jogo img = uncurry Translate (posMapToGlossNivel (cameraControl jogo) (posicao (jogador jogo))) $ if animacaoJogo jogo > 0 then scale (20*escala) (20*escala) img else scale 0 0 img
                    where escala = realToFrac (animacaoJogo jogo)*2+0.4

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Escada] (1*0.5,1*0.5) (mapa jogo)))

drawTunel :: Jogo -> Picture -> Picture
drawTunel jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Tunel] (1*0.5,1*0.5) (mapa jogo)))

drawPorta :: Jogo -> Picture -> Picture
drawPorta jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Porta] (1*0.5,1*0.5) (mapa jogo)))

drawEspinho :: Jogo -> Picture -> Picture
drawEspinho jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Espinho] (1*0.5,1*0.5) (mapa jogo)))



playAnim :: Float -> [Picture] -> Picture
playAnim time texs
    | (time `mod'` (1/n)) < 1/(n*2) = head texs
    | otherwise = texs !! 1
    where n = 6 -- Animation speed

--Any lenght Anim (usar por defenicao a length da lista como controlo do tempo)
playAnimAny :: Int -> Float -> [Picture] -> Picture
playAnimAny x time texs = texs !! frame
  where
    n = fromIntegral x -- Número de frames
    frame = floor (time * n) `mod` length texs

playDeadAnim :: State -> Picture -> Picture
playDeadAnim state = rotate ((2-animTime state)*360)

eventHandlerInGame :: Event -> Jogo -> Jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarDireita) jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarEsquerda) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Subir) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Descer) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo = atualiza (replicate (length (inimigos jogo)) Nothing) (Just Saltar) jogo
--eventHandlInGameer (EventKey (SpecialKey KeySpace) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Nothing) jogo
eventHandlerInGame e jogo = jogo

drawAlcapao :: Jogo -> Picture -> Picture
drawAlcapao jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (cameraControl jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Alcapao] (1*0.5,1*0.5) (mapa jogo)))

drawNum :: Int -> (Float, Float) -> State -> Picture
drawNum n (x,y) state = Pictures $ (foldl (\p c -> (Translate (x + (60*(fromIntegral $ length p))) y $
    case c of
        '1' -> um
        '2' -> dois
        '3' -> tres
        '4' -> quatro
        '5' -> cinco
        '6' -> seis
        '7' -> sete
        '8' -> oito
        '9' -> nove
        _ -> zero) : p) [] (show n))
    where um = fromJust (lookup "um" imagesTheme)
          dois = fromJust (lookup "dois" imagesTheme)
          tres = fromJust (lookup "tres" imagesTheme)
          quatro = fromJust (lookup "quatro" imagesTheme)
          cinco = fromJust (lookup "cinco" imagesTheme)
          seis = fromJust (lookup "seis" imagesTheme)
          sete = fromJust (lookup "sete" imagesTheme)
          oito = fromJust (lookup "oito" imagesTheme)
          nove = fromJust (lookup "nove" imagesTheme)
          zero = fromJust (lookup "zero" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          
