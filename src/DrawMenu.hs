module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Mapas
import Data.Maybe (fromJust)
import Utilities
import DrawLevel (drawNum)

-- | Faz o tratamento do input quando o utilizador se encontra no menu
eventHandlerInMenu :: Event -> State -> IO State
eventHandlerInMenu (EventKey (SpecialKey KeyDown) Down _ _) state = return state { menuState = (menuState state) {selectedButton = if selectedButton (menuState state)< menuArrowsLimit state then selectedButton (menuState state) + 1 else selectedButton (menuState state)}}
eventHandlerInMenu (EventKey (SpecialKey KeyUp) Down _ _) state = return state {menuState = (menuState state) {selectedButton = if selectedButton (menuState state)>0 then selectedButton (menuState state) - 1 else selectedButton (menuState state)}}
eventHandlerInMenu (EventKey (SpecialKey KeyLeft) Down _ _) state 
    | currentMenu state == LevelSelection = return state {menuState = (menuState state) {selectedButton = if selectedButton (menuState state)>0 then selectedButton (menuState state) - 1 else selectedButton (menuState state)}}
    | otherwise = return state
eventHandlerInMenu (EventKey (SpecialKey KeyRight) Down _ _) state
    | currentMenu state == LevelSelection = return state { menuState = (menuState state) {selectedButton = if selectedButton (menuState state)< menuArrowsLimit state then selectedButton (menuState state) + 1 else selectedButton (menuState state)}}
    | otherwise = return state
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {menuState = (menuState state) {pressingButton = True}}
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Up _ _) state = return $ if pressingButton $ menuState state then
        (buttonPress state) {menuState = (menuState (buttonPress state)) {pressingButton = False}}
    else
        state
--eventHandlerInMenu (EventKey (Char '1') Down _ _) state = return state {currentLevel = 1}
--eventHandlerInMenu (EventKey (Char '0') Down _ _) state = return state {currentLevel = 0}
eventHandlerInMenu e state = return state

menuArrowsLimit :: State -> Int
menuArrowsLimit state
    | currentMenu state == LevelSelection = length (levels state) - 1
    | otherwise = 3
-- | Função que deseha todos os elementos  visuais do menu
drawMenu :: State -> Picture
drawMenu state 
    | currentMenu state == MainMenu = Pictures [
        --drawTitle,
        drawButton (images state) "botaostart" (selectedButton (menuState state), 0) (pressingButton (menuState state)),
        drawButton (images state) "botaoSettings" (selectedButton (menuState state), 1) (pressingButton (menuState state)),
        drawButtonTextDebug (selectedButton (menuState state)) 2 "Editor",
        drawButton (images state) "botaoQuit" (selectedButton (menuState state), 3) (pressingButton (menuState state)),
        drawBanner (images state)
        ]
    | currentMenu state == OptionsMenu = Pictures [
        drawButtonTextDebug (selectedButton (menuState state)) 0 "Change theme"
    ]
    | currentMenu state == GameOver = Pictures [
        Color red $ scale 0.5 0.5 $ text "Game over!"
    ]
    | currentMenu state == LevelSelection = Pictures $ [drawBg state,
        scale 2.5 2.5 $ drawNum ((selectedButton $ menuState state) + 1) (0,25) state,
        drawArrow state
        ] -- ++
        -- map (\((level2, unlocked1), n) -> Pictures $
            -- [drawButtonTextDebug (selectedButton $ menuState state) n ("Jogo " ++ show n),
            -- drawLock unlocked1 n ]
            -- ) (zip (levels state) [0..]) --[(level1, n) | level1 <- levels state, n <- [0..length (levels state)-1]]

-- ! Remove
drawTitle :: Picture
drawTitle = Color blue $ Translate (-75) 100 $ Scale 0.3 0.3 $ text "Donkey kong"

-- | Desenha o fundo do menu
drawBg :: State -> Picture
drawBg state = scale (5*ratio) (5*ratio) $ img
    where img = fromJust $ lookup "bgMenu" (fromJust $ lookup Default (images state))
          ratio = (fromIntegral $ fst (screenSize state)) / (fromIntegral $ snd (screenSize state))

-- | Executa a função correspondente quando um determinado botão é pressionado
buttonPress :: State -> State
buttonPress state
    | selectedButton (menuState state) == 0 && currentMenu state == MainMenu = state { currentMenu = LevelSelection}
    | selectedButton (menuState state) == 1 && currentMenu state == MainMenu = state { currentMenu = OptionsMenu, menuState = (menuState state) {selectedButton = 0}}
    | selectedButton (menuState state) == 2 && currentMenu state == MainMenu = state {
        currentMenu = LevelEditor,
        editorState = (editorState state) {tempGame = jogo' {jogador = jog}, savingGame = False}}
    | selectedButton (menuState state) == 3 && currentMenu state == MainMenu = state { exitGame = True}
    | selectedButton (menuState state) == 0 && currentMenu state == OptionsMenu = state { options = (options state) {currentTheme = Minecraft} }
    | currentMenu state == LevelSelection = state { 
            currentMenu = InGame, 
            currentLevel = selectedButton (menuState state), 
            initLevel = jog',
            levels = replace (levels state) (selectedButton (menuState state), (jog', unlocked))
        }
    | otherwise = state
    where (jogo, unlocked) = (levels state) !! (selectedButton (menuState state))
          jogo' = initLevel state--(levels state) !! (currentLevel state)
          (Mapa (pos, dir) pos1 mat) = mapa jogo
          jog' = jogo {jogador = jog {posicao = pos}}

-- ! Remove (?)
-- | Desenha um botão, recebendo o indice atualmente desenhado, o indíce do próprio botão e o texto correspondente
drawButtonTextDebug :: Int -> Int -> String -> Picture
drawButtonTextDebug isEnabled n textButton = Pictures [
    (if isEnabled == n then Color green else Color white) $ Translate 0 (-110 -40 * fromIntegral n) $ rectangleWire 70 30,
    Color white $ Translate (-30) ((-110) -10 - 40 * fromIntegral n) $ Scale 0.2 0.2 $ Text textButton
    ]

drawButton :: Images -> String -> (Int, Int) -> Bool -> Picture
drawButton tex buttonType (currentIndex, index) pressed
    | currentIndex == index && pressed = Translate 0 (-70 + (-60 * fromIntegral index)) $ bTexPressed
    | currentIndex == index = Translate 0 (-70 + (-60 * fromIntegral index)) $ bTexHover
    | otherwise = Translate 0 (-70 + (-60 * fromIntegral index)) $ bTex
    where bTex = fromJust $ lookup buttonType (fromJust $ lookup Default tex)
          bTexHover = fromJust $ lookup (buttonType ++ "Hover") (fromJust $ lookup Default tex)
          bTexPressed = fromJust $ lookup (buttonType ++ "Pressed") (fromJust $ lookup Default tex)

drawBanner :: Images -> Picture
drawBanner tex = scale 1 1 $ fromJust $ lookup "menuBanner" (fromJust $ lookup Default tex)

drawArrow :: State -> Picture
drawArrow state = Pictures [
        if (selectedButton $ menuState state) > 0 then
            Translate (-200) 50 $ scale 2.5 2.5 $ fromJust $ lookup "arrow" (fromJust $ lookup Default (images state))
        else
            blank,
        if (selectedButton $ menuState state) < (menuArrowsLimit state) then
            Translate 200 50 $ scale (-2.5) 2.5 $ fromJust $ lookup "arrow" (fromJust $ lookup Default (images state))
        else
            blank
    ]

drawLock :: Bool -> Int -> Picture
drawLock unlocked n = Translate 90 (-10 + (-60 * fromIntegral n)) $ (if unlocked then Color green else Color red)
    $ circleSolid 18 --map (\(game, unlocked) -> )
