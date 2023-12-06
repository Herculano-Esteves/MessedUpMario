module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Mapas (State (selectedButton))

eventHandlerInMenu :: Event -> State -> IO State
eventHandlerInMenu (EventKey (SpecialKey KeyDown) Down _ _) state = return state {selectedButton = (selectedButton state) + 1}
eventHandlerInMenu (EventKey (SpecialKey KeyUp) Down _ _) state = return state {selectedButton = (selectedButton state) - 1}
eventHandlerInMenu e jogo = return jogo

drawMenu :: State -> Picture
drawMenu state = Pictures [
    drawTitle,
    drawButton (selectedButton state) 0 "Start",
    drawButton (selectedButton state) 1 "Start2",
    drawButton (selectedButton state) 2 "Start2"
    ]

drawTitle :: Picture
drawTitle = Color blue $ Translate (-70) 100 $ Scale 0.3 0.3 $ text "Donkey kong"

drawButton :: Int -> Int -> String -> Picture
drawButton isEnabled n textButton = Pictures [
    (if isEnabled == n then Color green else Color white) $ Translate 0 (-40 * fromIntegral n) $ rectangleWire 70 30,
    Color white $ Translate (-30) ((-10) - 40 * fromIntegral n) $ Scale 0.2 0.2 $ Text textButton
    ]