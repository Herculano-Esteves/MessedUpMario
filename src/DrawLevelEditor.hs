module DrawLevelEditor where
import Utilities
import Graphics.Gloss
import LI12324
import DrawLevel (posMapToGloss, drawMap, d2f)
import Tarefa1 (escalaGloss)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import LI12324 (Bloco(Plataforma))
import GHC.Float (double2Float)
import Mapas (jogoSamp)

reactLevelEditor :: Event -> State -> IO State
reactLevelEditor (EventKey (SpecialKey KeyUp) Down _ _) state = return state { levelEditorPos = (px, py - 1)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyDown) Down _ _) state = return state { levelEditorPos = (px, py + 1)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyRight) Down _ _) state = return state { levelEditorPos = (px+1, py)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyLeft) Down _ _) state = return state { levelEditorPos = (px-1, py)}
    where (px, py) = levelEditorPos state 
reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {levels = replace (levels state) ((currentLevel state),(replaceBlock (levelEditorPos state) jogo, unlocked))}
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
reactLevelEditor (EventKey (Char 'n') Down _ _) state = return $ addNewLevel state
reactLevelEditor e s = return s


drawLevelEditor :: State -> Picture
drawLevelEditor state = Pictures [
    drawMap jogo texPlataforma, 
    drawSelBox (levelEditorPos state)]
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)

drawSelBox :: Posicao -> Picture
drawSelBox pos = uncurry Translate (posMapToGloss pos) $ Color green $ rectangleWire (double2Float escalaGloss) (double2Float escalaGloss)

replaceBlock :: Posicao -> Jogo -> Jogo
replaceBlock (x,y) jog = replaceMapGame (x,y) newBlock jog
    where currentBlock = blocos !! floor y !! floor x
          (Mapa _ _ blocos) = mapa jog
          newBlock
            | currentBlock == Plataforma = Vazio
            | otherwise = Plataforma

addNewLevel :: State -> State
addNewLevel state = state {
    levels = levels state ++ [(emptyGame, True)],
    currentLevel = length (levels state)
}
    where emptyGame = jogoSamp {
            mapa = genEmptyMap (22,15)
          }

genEmptyMap :: (Int, Int) -> Mapa
genEmptyMap dim = Mapa ((0.5, 2.5), Oeste) (0.5, 5.5) (aux2 dim)
    where aux :: Int -> [Bloco]
          aux 0 = []
          aux x = Vazio : aux (x-1)
          aux2 :: (Int, Int) -> [[Bloco]]
          aux2 (_,0) = []
          aux2 (x,y) = aux x : aux2 (x,y-1)