module DrawMenu where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Mapas
import Data.Maybe (fromJust)
import Utilities
import DrawLevel

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
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Down _ _) state
    | currentMenu state == GameOver || currentMenu state == EndScreen = return state {currentMenu = MainMenu}
    | otherwise = return state {menuState = (menuState state) {pressingButton = True}}
eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Up _ _) state = return $ if pressingButton $ menuState state then
        (buttonPress state) {menuState = (menuState (buttonPress state)) {pressingButton = False}}
    else
        state
eventHandlerInMenu e state = return state

-- | Função que retorna o limite de setas que podem ser selecionadas num determinado menu
menuArrowsLimit :: State -> Int
menuArrowsLimit state
    | currentMenu state == LevelSelection = length (levels state) - 1
    | currentMenu state == InGame = 2
    | currentMenu state == OptionsMenu = 2
    | otherwise = 3

-- | Função que deseha todos os elementos  visuais do menu
drawMenu :: State -> Picture
drawMenu state 
    | currentMenu state == MainMenu = Pictures [
        drawEntityMenu state,
        Translate 0 (225) $ drawButton (images state) "botaostart" (selectedButton (menuState state), 0) (pressingButton (menuState state)),
        Translate 0 100 $ drawButton (images state) "botaoSettings" (selectedButton (menuState state), 1) (pressingButton (menuState state)),
        Translate 0 (-25) $ drawButton (images state) "botaoEdit" (selectedButton (menuState state), 2) (pressingButton (menuState state)),
        Translate 0 (-150) $ drawButton (images state) "botaoQuit" (selectedButton (menuState state), 3) (pressingButton (menuState state)),
        drawBanner (images state)
    ]
    | currentMenu state == OptionsMenu = Pictures [
        Translate 0 350 $ scale 2 2 $ temasText,
        drawMarioThemeSel state,
        Translate 0 100 $ drawButton (images state) "botaoQuit" (selectedButton (menuState state), 2) (pressingButton (menuState state)),
        drawCredits state
    ]
    | currentMenu state == GameOver = Pictures [
        -- Color red $ scale 0.5 0.5 $ text "Game over!"
        drawGameover state
    ]
    | currentMenu state == EndScreen = Pictures [
        drawEndScreen state
    ]
    | currentMenu state == LevelSelection = Pictures $ [drawBg state,
        scale 2.5 2.5 $ drawNum ((selectedButton $ menuState state) + 1) (0,25) state,
        drawArrow state,
        drawLock state,
        drawHighscore state
        ] 
    where temasText = fromJust $ lookup "temasText" (fromJust $ lookup Default (images state))
          
drawEntityMenu :: State -> Picture
drawEntityMenu state = Pictures $ [Translate (-1000) (20) $ scale (-15) 15 $ playAnimAny (9) (time state) allanimation]
        where   allanimation =  bossanim ++ bossanim ++ bossanim ++ bossanim ++ bossanim ++ bossanim ++ ataqueboss
                bossanim = [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6]
                texBoss1 = fromJust (lookup "boss1" imagesTheme)
                texBoss2 = fromJust (lookup "boss2" imagesTheme)
                texBoss3 = fromJust (lookup "boss3" imagesTheme)
                texBoss4 = fromJust (lookup "boss4" imagesTheme)
                texBoss5 = fromJust (lookup "boss5" imagesTheme)
                texBoss6 = fromJust (lookup "boss6" imagesTheme)
                ataqueboss = [texataque1,texataque2,texataque3,texataque4,texataque5,texataque6,texataque7,texataque8,texataque9,texataque10]
                texataque1 = fromJust (lookup "ataqueboss1" imagesTheme)
                texataque3 = fromJust (lookup "ataqueboss3" imagesTheme)
                texataque4 = fromJust (lookup "ataqueboss4" imagesTheme)
                texataque5 = fromJust (lookup "ataqueboss5" imagesTheme)
                texataque2 = fromJust (lookup "ataqueboss2" imagesTheme)
                texataque6 = fromJust (lookup "ataqueboss6" imagesTheme)
                texataque7 = fromJust (lookup "ataqueboss7" imagesTheme)
                texataque8 = fromJust (lookup "ataqueboss8" imagesTheme)
                texataque9 = fromJust (lookup "ataqueboss9" imagesTheme)
                texataque10 = fromJust (lookup "ataqueboss10" imagesTheme)
                imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))


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
    | selectedButton (menuState state) == 0 && currentMenu state == OptionsMenu = state { options = (options state) {marioTheme = switchTheme 0 (marioTheme $ options state)} }
    | selectedButton (menuState state) == 1 && currentMenu state == OptionsMenu = state { options = (options state) {platformTheme = switchTheme 1 (platformTheme $ options state)} }
    | selectedButton (menuState state) == 2 && currentMenu state == OptionsMenu = state { currentMenu = MainMenu }
    | currentMenu state == LevelSelection && (unlocked || cheats state) = state { 
            currentMenu = InGame, 
            currentLevel = selectedButton (menuState state), 
            initLevel = jog',
            levels = replace (levels state) (selectedButton (menuState state), (jog', unlocked)),
            menuState = (menuState state) {selectedButton = 0}
        }
    | currentMenu state == LevelSelection = state
    -- InGame pause menu
    | selectedButton (menuState state) == 0 && currentMenu state == InGame = state {
        levels = replace (levels state) (currentLevel state, (jogoCurrent {lostGame = 3}, unlockedCurrent)),
        menuState = (menuState state) {selectedButton = 0}
    }
    | selectedButton (menuState state) == 1 && currentMenu state == InGame = state {
        levels = replace (levels state) (currentLevel state, (jogoCurrent {lostGame = 4}, unlockedCurrent)),
        menuState = (menuState state) {selectedButton = 0}
    }
    | selectedButton (menuState state) == 2 && currentMenu state == InGame = state {
        levels = replace (levels state) (currentLevel state,(initLevel state, unlockedCurrent)),
        currentMenu = MainMenu
    }
    | otherwise = state
    where (jogo, unlocked) = (levels state) !! (selectedButton (menuState state))
          (jogoCurrent, unlockedCurrent) = (levels state) !! (currentLevel state)
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

-- | Desenha o background do menu principal
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

-- | Desenha um cadeado aberto ou fechado, dependendo se o nível está desbloqueado ou não
drawLock :: State -> Picture
drawLock state = Translate 20 (-100) $ (if unlocked then lockOpen else lockClosed)
    where lockOpen = fromJust $ lookup "lockOpen" (fromJust $ lookup Default (images state))
          lockClosed = fromJust $ lookup "lockClosed" (fromJust $ lookup Default (images state))
          unlocked = snd $ (levels state) !! (selectedButton $ menuState state)

-- | Desenha o ecrã de game over
drawGameover :: State -> Picture
drawGameover state = Pictures [
        scale 7.5 7.5 $ tex,
        Translate 0 (snd $ posMapToGloss state (0,10)) $ scale 1.5 1.5 $ pressEnterTex
    ]
    where tex = fromJust $ lookup "gameOver" (fromJust $ lookup Default (images state))
          pressEnterTex = fromJust $ lookup "pressEnterText" (fromJust $ lookup Default (images state))

-- | Desenha o ecrã de fim de jogo
drawEndScreen :: State -> Picture
drawEndScreen state = Pictures [
        scale 10 10 $ tex,
        Translate 0 (snd $ posMapToGloss state (0,10)) $ scale 1.5 1.5 $ pressEnterTex
    ]
    where tex = fromJust $ lookup "endScreen" (fromJust $ lookup Default (images state))
          pressEnterTex = fromJust $ lookup "pressEnterText" (fromJust $ lookup Default (images state))

-- | Altera o tema atual
switchTheme :: Int -> Theme -> Theme
switchTheme n current
    | n == 0 = case current of
        Default -> Quadradinho
        Quadradinho -> Caverna
        Caverna -> Gold
        Gold -> Default
    | n == 1 = case current of
        Default -> Minecraft
        Minecraft -> Default

-- | Desenha o tema atual
drawMarioThemeSel :: State -> Picture
drawMarioThemeSel state = Pictures [
    Translate 0 (200) $ (if (marioTheme $ options state) /= Caverna then
            scale 5 5
        else
            scale 3.5 3.5)
        currentMario,
    Translate 0 (25) $ scale 2 2 currentPlatform,
    -- drawArrow state
    Translate (-100) (200 -175 * index) $ scale (-1) 1 $ arrowTex
    ]
    where currentMario = fromJust $ lookup "marioandar1" (fromJust $ lookup (marioTheme $ options state) (images state))
          currentPlatform = fromJust $ lookup "plataforma" (fromJust $ lookup (platformTheme $ options state) (images state))
          arrowTex = fromJust $ lookup "arrow" (fromJust $ lookup Default (images state))
          index
            | (selectedButton $ menuState state) > 1 = 1
            | otherwise = (fromIntegral $ selectedButton $ menuState state)

-- | Desenha o highscore
drawHighscore :: State -> Picture
drawHighscore state = Pictures [
    Translate 0 (-450) $ scale 2.5 2.5 $ highscoreTex,
    scale 1 1 $ drawNum (highscore state) (25,-450) state
    ]
    where highscoreTex = fromJust $ lookup "highscoreText" (fromJust $ lookup Default (images state))

drawCredits :: State -> Picture
drawCredits state = Translate 0 (-300) $ scale 1 1 $ creditsText
    where creditsText = fromJust $ lookup "creditsText" (fromJust $ lookup Default (images state))