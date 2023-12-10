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

-- | Devolve o tamanho da janela apropriado para um determinado mapa inicial e uma escala dos blocos
sizeWin :: (Int, Int)
sizeWin = (round $ snd (snd (getMapaDimensoes escalaGloss mapaTeste)), round $ fst $ (snd (getMapaDimensoes escalaGloss mapaTeste)))

-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss
posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)

d2f = double2Float
f2d = float2Double

drawLevel :: State -> Picture
drawLevel state = Pictures ([drawLadder (jogo state) texEscada,drawEnemies texInimigo (jogo state), drawPlayer (mapa(jogo state)) texMariocair texMariosaltar texMarioandar (jogador (jogo state))] ++ drawMap (jogo state) texPlataforma ++ drawColecs texMoeda texMartelo (jogo state) ++ [drawAlcapao (jogo state) texAlcapao] ++ [drawTunel (jogo state) texTunel] ++
                ([drawHammer texMartelo (jogador (jogo state)) | fst (aplicaDano (jogador (jogo state)))]))
    where texEscada = fromJust (lookup "escada" (images state))
          texMarioandar = fromJust (lookup "marioandar" (images state))
          texMariosaltar = fromJust (lookup "mariosaltar" (images state))
          texPlataforma = fromJust (lookup "plataforma" (images state))
          texAlcapao = fromJust (lookup "alcapao" (images state))
          texTunel = fromJust (lookup "tunel" (images state))
          texInimigo = fromJust (lookup "inimigo" (images state))
          texMoeda = fromJust (lookup "moeda" (images state))
          texMartelo = fromJust (lookup "martelo" (images state))
          texMariocair = fromJust (lookup "mariocair" (images state))

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Mapa -> Picture -> Picture -> Picture -> Personagem -> Picture
drawPlayer mapa pixcair picsaltar picandar jog = Translate (((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2) (scale (if direcao jog == Este then 1 else -1) 1
                                            (if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then picandar else
                                            (if snd (velocidade jog) == 0 then picandar else
                                            (if fst (velocidade jog) == 0 then pixcair else picsaltar)))) --rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float escalaGloss)) ((double2Float $ snd $ tamanho jog)*double2Float escalaGloss)

-- (if (fst(velocidade jog) == 4 || fst(velocidade jog) == (-4)) && snd(velocidade jog) >= 0 && snd(velocidade jog) <= 1 then picandar else
drawEnemies :: Picture -> Jogo -> Picture
drawEnemies tex jogo = Pictures $ map (drawEnemy tex) (inimigos jogo)

drawEnemy :: Picture -> Personagem -> Picture
drawEnemy tex inim = Color yellow $ Translate (fst $ posMapToGloss (posicao inim)) (0.3+(snd $ posMapToGloss (posicao inim))) $ Scale 0.85 0.85 tex

drawColecs :: Picture -> Picture -> Jogo -> [Picture]
drawColecs moeda martelo jogo = map (\(colec,pos) -> if colec == Moeda then Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) $ (Color red) (Scale 0.6 0.6 moeda) else Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) $ (Color red) martelo) (colecionaveis jogo)

drawHammer :: Picture -> Personagem -> Picture
drawHammer tex jog = Color yellow $ Translate (if direcao jog == Este then p1 + (double2Float (fst (tamanho jog))*double2Float (escalaGloss)) else p1 - (double2Float (fst (tamanho jog)))*double2Float (escalaGloss)) p2 tex
                    where   p1 = ((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2
                            p2 = (-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2

-- TODO: Maybe we should make the get center of hitbox not receive a scale to avoid having to set it to 1
drawMap :: Jogo -> Picture -> [Picture]
drawMap jogo img = map (\pos -> Color white $ Translate (fst $ posMapToGloss pos) (snd $ posMapToGloss pos) img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) ++
    map (\pos -> Color white $ uncurry Translate (posMapToGloss pos) $ Color green $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Escada] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawTunel :: Jogo -> Picture -> Picture
drawTunel jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Tunel] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))


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
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
