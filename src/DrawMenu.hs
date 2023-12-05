module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

eventHandlerInMenu :: Event -> Jogo -> IO Jogo
eventHandlerInMenu e jogo = return jogo

drawMenu :: Picture
drawMenu = Pictures [
    drawTitle,
    drawButton True "Start"
    ]

drawTitle :: Picture
drawTitle = Color blue $ Translate (-70) 100 $ Scale 0.3 0.3 $ text "Donkey kong"

drawButton :: Bool -> String -> Picture
drawButton isEnabled textButton = Pictures [
    Color green $ rectangleWire 70 30,
    Color white $ Translate (-30) (-10) $ Scale 0.2 0.2 $ Text textButton
    ]