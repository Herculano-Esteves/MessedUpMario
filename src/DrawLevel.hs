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
import LI12324
import Data.Maybe (fromJust)

-- | Devolve o tamanho da janela apropriado para um determinado mapa inicial e uma escala dos blocos
sizeWin :: (Int, Int)
sizeWin = (round $ snd $ (snd (getMapaDimensoes escalaGloss mapaTeste)), (round $ fst $ (snd (getMapaDimensoes escalaGloss mapaTeste))))

-- | Faz a conversão do refrencial usado na lógica interna do jogo para o referencial usado pelo gloss
posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = ((d2f x*d2f escalaGloss)-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)

d2f = double2Float
f2d = float2Double

drawLevel :: State -> Picture
drawLevel state = Pictures ([drawLadder (jogo state) texEscada, drawPlayer  texMario (jogador (jogo state))] ++ (drawMap (jogo state) texPlataforma) ++ drawColecs (jogo state) ++ [drawAlcapao (jogo state) texAlcapao])
    where texEscada = fromJust(lookup "escada" (images state))
          texMario = fromJust(lookup "mario" (images state))
          texPlataforma = fromJust(lookup "plataforma" (images state))
          texAlcapao = fromJust(lookup "alcapao" (images state))

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Picture -> Personagem -> Picture
drawPlayer pic jog = Color red $ Translate (((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2) $ pic --rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float escalaGloss)) ((double2Float $ snd $ tamanho jog)*double2Float escalaGloss)

drawColecs :: Jogo -> [Picture]
drawColecs jogo = map (\(colec,pos) -> (Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) ) $ (Color red) $ (rectangleSolid 25 25)) (colecionaveis jogo)

-- TODO: Maybe we should make the get center of hitbox not receive a scale to avoid having to set it to 1
drawMap :: Jogo -> Picture -> [Picture]
drawMap jogo img = map (\pos -> Color white $ Translate (fst $ posMapToGloss pos) (snd $ posMapToGloss pos) $ img) (getcenterofhitbox 1 (getMapColisions 1 [Plataforma] (1*0.5,1*0.5) (mapa jogo))) ++
    map (\pos -> Color white $ Translate (fst $ posMapToGloss pos) (snd $ posMapToGloss pos) $ Color green $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Escada] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

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
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))
