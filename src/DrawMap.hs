module DrawMap where


import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Graphics.Gloss

import GHC.Float (float2Double, double2Float)

sizeWin :: (Int, Int)
sizeWin = (round $ snd $ (snd (getMapaDimensoes escalaGloss mapaTeste)), (round $ fst $ (snd (getMapaDimensoes escalaGloss mapaTeste))))

posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (((double2Float x)*d2f escalaGloss)-(fromIntegral $(fst sizeWin))/2, ((fromIntegral $ (snd sizeWin))/2 - (double2Float y) * d2f escalaGloss))

d2f = double2Float
f2d = float2Double

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Picture -> Personagem -> Picture
drawPlayer pic jog = Color red $ Translate (((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2) $ pic --rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float escalaGloss)) ((double2Float $ snd $ tamanho jog)*double2Float escalaGloss)

drawColecs :: Jogo -> [Picture]
drawColecs jogo = map (\(colec,pos) -> (Translate (fst $ (posMapToGloss pos)) (snd $ (posMapToGloss pos)) ) $ (Color red) $ (rectangleSolid 25 25)) (colecionaveis jogo)

drawLs :: Jogo -> Picture -> [Picture]
drawLs jogo img = map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Plataforma] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo))) ++
    map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ Color blue $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

drawLadder :: Jogo -> Picture -> Picture
drawLadder jogo img = Pictures $ map (\(x,y) -> Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Escada] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))