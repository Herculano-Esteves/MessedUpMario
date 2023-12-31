module DrawLevelEditor where
import Utilities
import Graphics.Gloss
import LI12324
import DrawLevel (posMapToGloss, drawMap)
import Tarefa1 (escalaGloss)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Game (SpecialKey(KeyLeft))
import LI12324 (Bloco(Plataforma))

reactLevelEditor :: Event -> State -> IO State
reactLevelEditor (EventKey (SpecialKey KeyUp) Down _ _) state = return state { levelEditorPos = (px, py - 1)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyDown) Down _ _) state = return state { levelEditorPos = (px, py + 1)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyRight) Down _ _) state = return state { levelEditorPos = (px+1, py)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyLeft) Down _ _) state = return state { levelEditorPos = (px-1, py)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {levels = replace (levels state) ((currentLevel state),(replaceMapGame (levelEditorPos state) jogo, unlocked))}
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
reactLevelEditor e s = return s


drawLevelEditor :: State -> Picture
drawLevelEditor state = Pictures [
    drawMap jogo texPlataforma, 
    drawSelBox (levelEditorPos state)]
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)

drawSelBox :: Posicao -> Picture
drawSelBox pos = uncurry Translate (posMapToGloss pos) $ Color green $ rectangleWire 50 50