module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Mapas

-- | Faz o tratamento do input quando o utilizador se encontra no menu
eventHandlerInMenu :: Event -> State -> IO State
eventHandlerInMenu (EventKey (SpecialKey KeyDown) Down _ _) state = return state {selectedButton = if selectedButton state<2 then (selectedButton state) + 1 else selectedButton state}
eventHandlerInMenu (EventKey (SpecialKey KeyUp) Down _ _) state = return state {selectedButton = if selectedButton state>0 then (selectedButton state) - 1 else selectedButton state}
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Down _ _) state = return (buttonPress state)
eventHandlerInMenu e jogo = return jogo

-- | Função que deseha todos os elementos  visuais do menu
drawMenu :: State -> Picture
drawMenu state = Pictures [
    drawTitle,
    drawButton (selectedButton state) 0 "Start",
    drawButton (selectedButton state) 1 "Exit",
    drawButton (selectedButton state) 2 "Start2"
    ]

drawTitle :: Picture
drawTitle = Color blue $ Translate (-75) 100 $ Scale 0.3 0.3 $ text "Donkey kong"

-- | Executa a função correspondente quando um determinado botão é pressionado
buttonPress :: State -> State
buttonPress state
    | selectedButton state == 0 = state { inGame = True}
    | selectedButton state == 1 = state { exitGame = True}
    | otherwise = state

-- | Desenha um botão, recebendo o indice atualmente desenhado, o indíce do próprio botão e o texto correspondente
drawButton :: Int -> Int -> String -> Picture
drawButton isEnabled n textButton = Pictures [
    (if isEnabled == n then Color green else Color white) $ Translate 0 (-40 * fromIntegral n) $ rectangleWire 70 30,
    Color white $ Translate (-30) ((-10) - 40 * fromIntegral n) $ Scale 0.2 0.2 $ Text textButton
    ]