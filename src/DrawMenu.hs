module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Mapas
import Data.Maybe (fromJust)

-- | Faz o tratamento do input quando o utilizador se encontra no menu
eventHandlerInMenu :: Event -> State -> IO State
eventHandlerInMenu (EventKey (SpecialKey KeyDown) Down _ _) state = return state {selectedButton = if selectedButton state<2 then (selectedButton state) + 1 else selectedButton state}
eventHandlerInMenu (EventKey (SpecialKey KeyUp) Down _ _) state = return state {selectedButton = if selectedButton state>0 then (selectedButton state) - 1 else selectedButton state}
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Down _ _) state = return (buttonPress state)
eventHandlerInMenu e jogo = return jogo

-- | Função que deseha todos os elementos  visuais do menu
drawMenu :: State -> Picture
drawMenu state 
    | currentMenu state == MainMenu = Pictures [
        --drawTitle,
        drawButton (images state) "botaostart" (selectedButton state) 0,
        drawButton (images state) "botaoSettings" (selectedButton state) 1,
        drawButton (images state) "botaoQuit" (selectedButton state) 2,
        drawBanner (images state)
        ]
    | currentMenu state == OptionsMenu = Pictures [
        drawButtonTextDebug (selectedButton state) 0 "Change theme"
    ]

-- ! Remove
drawTitle :: Picture
drawTitle = Color blue $ Translate (-75) 100 $ Scale 0.3 0.3 $ text "Donkey kong"

-- | Executa a função correspondente quando um determinado botão é pressionado
buttonPress :: State -> State
buttonPress state
    | selectedButton state == 0 && currentMenu state == MainMenu = state { currentMenu = InGame}
    | selectedButton state == 0 && currentMenu state == OptionsMenu = state { options = (options state) {currentTheme = Minecraft} }
    | selectedButton state == 1 = state { currentMenu = OptionsMenu, selectedButton = 0}
    | selectedButton state == 2 = state { exitGame = True}
    | otherwise = state

-- ! Remove (?)
-- | Desenha um botão, recebendo o indice atualmente desenhado, o indíce do próprio botão e o texto correspondente
drawButtonTextDebug :: Int -> Int -> String -> Picture
drawButtonTextDebug isEnabled n textButton = Pictures [
    (if isEnabled == n then Color green else Color white) $ Translate 0 (-40 * fromIntegral n) $ rectangleWire 70 30,
    Color white $ Translate (-30) ((-10) - 40 * fromIntegral n) $ Scale 0.2 0.2 $ Text textButton
    ]

drawButton :: Images -> String -> Int -> Int -> Picture
drawButton tex buttonType currentIndex index
    | currentIndex == index = Translate 0 (-70 + (-60 * fromIntegral index)) $ bTexHover
    | otherwise = Translate 0 (-70 + (-60 * fromIntegral index)) $ bTex
    where bTex = fromJust $ lookup buttonType (fromJust $ lookup Default tex)
          bTexHover = fromJust $ lookup (buttonType ++ "Hover") (fromJust $ lookup Default tex)

drawBanner :: Images -> Picture
drawBanner tex = scale 0.75 0.75 $ fromJust $ lookup "menuBanner" (fromJust $ lookup Default tex)