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

-- | Devolve o tamanho da janela apropriado para um determinado mapa inicial e uma escala dos blocos
sizeWin :: (Int, Int)
sizeWin = (round $ snd (snd (getMapaDimensoes escalaGloss mapaTeste)), round $ fst $ (snd (getMapaDimensoes escalaGloss mapaTeste)))

-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss
posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)

d2f = double2Float
f2d = float2Double

drawLevel :: State -> Picture
drawLevel state = Pictures ([drawLadder jogo texEscada,drawEnemies texInimigo jogo] ++ [drawPorta jogo texMoeda]  ++ drawMap jogo texPlataforma ++ drawColecs texMoeda texMartelo jogo ++ [drawAlcapao jogo texAlcapao] ++ [drawTunel jogo texTunel] ++
                ([drawHammer texMartelo (jogador jogo) | fst (aplicaDano (jogador jogo))]) ++ [drawPlayer (mapa jogo) texMariocair texMariosaltar texMarioandar (jogador jogo)])
    where texEscada = fromJust (lookup "escada" imagesTheme)
          texMarioandar = fromJust (lookup "marioandar" imagesTheme)
          texMariosaltar = fromJust (lookup "mariosaltar" imagesTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo = fromJust (lookup "inimigo" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texMartelo = fromJust (lookup "martelo" imagesTheme)
          texMariocair = fromJust (lookup "mariocair" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          jogo = fromJust (lookup (currentLevel state) (levels state))


-- ? Set a scale for drawng according to the size of the window
-- TODO: Check if the code "$ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $" is actually working properly
drawPlayer :: Mapa -> Picture -> Picture -> Picture -> Personagem -> Picture
drawPlayer mapa pixcair picsaltar picandar jog = Translate (fst $ posMapToGloss (posicao jog)) (snd $ posMapToGloss (posicao jog)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $
    scale (if direcao jog == Este then 1 else -1) 1 $
    if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then
        picandar
    else if snd (velocidade jog) == 0 then
        picandar
    else (if fst (velocidade jog) == 0 then pixcair else picsaltar)

-- (if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then picandar else
drawEnemies :: Picture -> Jogo -> Picture
drawEnemies tex jogo = Pictures $ map (drawEnemy tex) (inimigos jogo)

-- TODO: Also check scale here
drawEnemy :: Picture -> Personagem -> Picture
drawEnemy tex inim = Translate (fst $ posMapToGloss (posicao inim)) (0.3+(snd $ posMapToGloss (posicao inim))) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ Scale 0.85 0.85 tex

drawColecs :: Picture -> Picture -> Jogo -> [Picture]
drawColecs moeda martelo jogo = map (\(colec,pos) -> if colec == Moeda then Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ (Scale 0.6 0.6 moeda) else Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ martelo) (colecionaveis jogo)

drawHammer :: Picture -> Personagem -> Picture
drawHammer tex jog = Color yellow $ Translate (if direcao jog == Este then p1 + (double2Float (fst (tamanho jog))*double2Float (escalaGloss)) else p1 - (double2Float (fst (tamanho jog)))*double2Float (escalaGloss)) p2 $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ tex
                    where   p1 = ((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2
                            p2 = (-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2

-- TODO: Maybe we should make the get center of hitbox not receive a scale to avoid having to set it to 1
drawMap :: Jogo -> Picture -> [Picture]
drawMap jogo img = map (\pos -> Color white $ Translate (fst $ posMapToGloss pos) (snd $ posMapToGloss pos) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) -- ++
    --map (\pos -> Color white $ uncurry Translate (posMapToGloss pos) $ Color green $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Escada] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawTunel :: Jogo -> Picture -> Picture
drawTunel jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Tunel] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawPorta :: Jogo -> Picture -> Picture
drawPorta jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Porta] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))


eventHandlerInGame :: Event -> Jogo -> Jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
--eventHandlInGameer (EventKey (SpecialKey KeySpace) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Nothing) jogo
eventHandlerInGame e jogo = jogo

drawAlcapao :: Jogo -> Picture -> Picture
drawAlcapao jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
