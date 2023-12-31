module DrawLevel where


import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import GHC.Float (float2Double, double2Float)
import Mapas
import Data.Maybe (fromJust)
import Utilities
import Data.Fixed (mod')

-- | Devolve o tamanho da janela apropriado para um determinado mapa inicial e uma escala dos blocos
sizeWin :: (Int, Int)
sizeWin = (round $ snd (snd (getMapaDimensoes escalaGloss (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)))), round $ fst $ (snd (getMapaDimensoes escalaGloss (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)))))

-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss
posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)

posMapToGlossNivel :: Personagem -> Posicao -> (Float,Float)
posMapToGlossNivel jogador (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)





d2f = double2Float
f2d = float2Double

drawLevel :: State -> Picture
drawLevel state = Pictures [drawLadder jogo texEscada, drawPorta jogo texPorta, drawMap jogo texPlataforma, drawColecs texMoeda texMartelo texChave jogo, drawAlcapao jogo texAlcapao, drawTunel jogo texTunel,
                if fst $ aplicaDano (jogador jogo) then drawHammer texMartelo (jogador jogo) else blank, drawPlayer state (jogador jogo),drawEnemies texInimigo texMacaco texBarril jogo,drawMorte jogo texMorte]
    where texEscada = fromJust (lookup "escada" imagesTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo = fromJust (lookup "inimigo" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texMartelo = fromJust (lookup "martelo" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesTheme)
          texPorta = fromJust (lookup "portaMario" imagesTheme)
          texMacaco = fromJust (lookup "macacoMalvado" imagesTheme)
          texBarril = fromJust (lookup "barril" imagesTheme)
          texMorte = fromJust (lookup "morreu" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          (jogo, unlocked) = (levels state) !! (currentLevel state)


-- ? Set a scale for drawng according to the size of the window
-- TODO: Check if the code "$ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $" is actually working properly
drawPlayer :: State -> Personagem -> Picture
<<<<<<< HEAD
drawPlayer state jog = uncurry Translate (posMapToGlossNivel jog (posicao jog)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
=======
drawPlayer state jog = uncurry Translate (posMapToGloss (posicao jog)) $ playDeadAnim state $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
>>>>>>> nuno
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
          escala = realToFrac(snd(aplicaDano jog))

-- (if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then picandar else
drawEnemies :: Picture -> Picture -> Picture -> Jogo -> Picture
drawEnemies texinimigo texMacaco texBarril jogo = Pictures $ map (\x ->if tipo x == Fantasma then drawEnemy  texinimigo x (jogador jogo) else
                                                            if tipo x == MacacoMalvado then drawEnemy texMacaco x (jogador jogo) else drawEnemy texBarril x (jogador jogo)) (inimigos jogo)

drawEnemy :: Picture -> Personagem -> Personagem -> Picture
drawEnemy tex inim jogador = Pictures [Translate (fst $ posMapToGlossNivel jogador (posicao inim)) (0.3+(snd $ posMapToGlossNivel jogador (posicao inim))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ Rotate (if tipo inim == Barril then (fromInteger(floor (snd(posicao inim))))*90 else 0) $ Scale (if fst(velocidade inim) > 0 then 1 else -1) 1 tex, drawHitbox jogador inim]

drawHitbox :: Personagem -> Personagem -> Picture
drawHitbox jogador inm = Color green $ uncurry Translate (posMapToGlossNivel jogador (posicao inm)) $ rectangleWire tx ty
    where tx = (fst $ snd $ aux (genHitbox inm)) - (fst $ fst $ aux (genHitbox inm))
          ty = (snd $ snd $ aux (genHitbox inm)) - (snd $ fst $ aux (genHitbox inm))
          aux :: Hitbox -> ((Float,Float),(Float,Float))
          aux (p1,p2) = (posMapToGlossNivel jogador p1, posMapToGlossNivel jogador p2)

drawColecs :: Picture -> Picture -> Picture -> Jogo -> Picture
drawColecs moeda martelo chave jogo = Pictures $ map (\(colec,pos) -> if colec == Moeda then Translate (fst $ (posMapToGlossNivel (jogador jogo) pos)) (snd $ (posMapToGlossNivel (jogador jogo) pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ (Scale 0.7 0.7 moeda) else 
                                                     if colec == Martelo then Translate (fst $ (posMapToGlossNivel (jogador jogo) pos)) (snd $ (posMapToGlossNivel (jogador jogo) pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ martelo else 
                                                     if colec == Chave then Translate (fst $ (posMapToGlossNivel (jogador jogo) pos)) (snd $ (posMapToGlossNivel (jogador jogo) pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ chave else
                                                     Translate (fst $ (posMapToGlossNivel (jogador jogo) pos)) (snd $ (posMapToGlossNivel (jogador jogo) pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ martelo) 

                                (colecionaveis jogo)

drawHammer :: Picture -> Personagem -> Picture
drawHammer tex jog = Color yellow $ Translate (if direcao jog == Este then p1 + (double2Float (fst (tamanho jog))*double2Float (escalaGloss)) else p1 - (double2Float (fst (tamanho jog)))*double2Float (escalaGloss)) p2 $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ tex
                    where   p1 = ((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2
                            p2 = (-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2

-- TODO: Maybe we should make the get center of hitbox not receive a scale to avoid having to set it to 1
drawMap :: Jogo -> Picture -> Picture
drawMap jogo img = Pictures $ map (\pos -> Color white $ uncurry Translate (posMapToGlossNivel (jogador jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) -- ++
    --map (\pos -> Color white $ uncurry Translate (posMapToGloss pos) $ Color green $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
drawMorte :: Jogo -> Picture -> Picture
drawMorte jogo img = uncurry Translate (posMapToGlossNivel (jogador jogo) (posicao (jogador jogo))) $ if snd (aplicaDano (jogador jogo)) > 0 && not (fst (aplicaDano (jogador jogo))) then scale (20*escala) (20*escala) img else scale 0 0 img
                    where escala = realToFrac(snd(aplicaDano (jogador jogo)))-16.7

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (jogador jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Escada] (1*0.5,1*0.5) (mapa jogo)))

drawTunel :: Jogo -> Picture -> Picture
drawTunel jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (jogador jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Tunel] (1*0.5,1*0.5) (mapa jogo)))

drawPorta :: Jogo -> Picture -> Picture
drawPorta jogo img = Pictures $ map (\pos -> uncurry Translate (posMapToGlossNivel (jogador jogo) pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) img) (getcenterofhitbox 1 (getMapColisions 1 [Porta] (1*0.5,1*0.5) (mapa jogo)))

playAnim :: Float -> [Picture] -> Picture
playAnim time texs
    | (time `mod'` (1/n)) < (1/(n*2)) = texs !! 0
    | otherwise = texs !! 1
    where n = 6 -- Animation speed

playDeadAnim :: State -> Picture -> Picture
playDeadAnim state tex = rotate ((2-(animTime state))*360) tex

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
drawAlcapao jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
