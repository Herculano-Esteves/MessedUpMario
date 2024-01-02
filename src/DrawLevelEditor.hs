module DrawLevelEditor where
import Utilities
import Graphics.Gloss
import LI12324
import DrawLevel
import Tarefa1 (escalaGloss)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import LI12324 (Bloco(Plataforma))
import GHC.Float (double2Float)
import Mapas (jogoSamp)
import Tarefa2 (floorPos)

reactLevelEditor :: Event -> State -> IO State
reactLevelEditor (EventKey (SpecialKey KeyUp) Down _ _) state = return state {
        editorState = (editorState state) {levelEditorPos = (px, py - 1)}
    }
    where (px, py) = levelEditorPos $ editorState state 
reactLevelEditor (EventKey (SpecialKey KeyDown) Down _ _) state = return state {
        editorState = (editorState state) {levelEditorPos = (px, py + 1)}
    }
    where (px, py) = levelEditorPos $ editorState state
reactLevelEditor (EventKey (SpecialKey KeyRight) Down _ _) state = return state {
        editorState = (editorState state) {levelEditorPos = (px+1, py)}
    }
    where (px, py) = levelEditorPos $ editorState state
reactLevelEditor (EventKey (SpecialKey KeyLeft) Down _ _) state = return state {
        editorState = (editorState state) {levelEditorPos = (px-1, py)}
    }
    where (px, py) = levelEditorPos $ editorState state
reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {
        levels = if changingBlocks $ editorState state then
                replace (levels state) ((currentLevel state),(replaceBlock (levelEditorPos $ editorState state) jogo, unlocked))
                else
                    replace (levels state) ((currentLevel state),(switchEnemy (levelEditorPos $ editorState state) jogo, unlocked))
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
reactLevelEditor (EventKey (Char 'a') Down _ _) state = return state {
        levels = replace (levels state) ((currentLevel state),(addRemoveEnemy (levelEditorPos $ editorState state) jogo, unlocked))
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
reactLevelEditor (EventKey (Char 's') Down _ _) state = return state {
    editorState = (editorState state) {changingBlocks = if (changingBlocks $ editorState state) then False else True}}
reactLevelEditor (EventKey (Char 'n') Down _ _) state = return $ addNewLevel state
reactLevelEditor e s = return s


drawLevelEditor :: State -> Picture
drawLevelEditor state = Pictures [drawLadder jogo texEscada, drawPorta jogo texPorta, drawMap jogo texPlataforma, drawColecs texMoeda texMartelo texChave jogo, drawAlcapao jogo texAlcapao, drawTunel jogo texTunel,
                drawPlayer state (jogador jogo),drawEnemies (texCuspo1,texCuspo2) texInimigo texMacaco texBarril texBoss jogo,drawMorte jogo texMorte, drawSelBox state (levelEditorPos $ editorState state)]
    where texEscada = fromJust (lookup "escada" imagesTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo = fromJust (lookup "inimigo" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texMartelo = fromJust (lookup "martelo" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesTheme)
          texPorta = fromJust (lookup "portaMario" imagesTheme)
          texMacaco = fromJust (lookup "macacoMalvado" imagesTheme)
          texBarril = fromJust (lookup "barril" imagesTheme)
          texMorte = fromJust (lookup "morreu" imagesTheme)
          texBoss = fromJust (lookup "boss1" imagesTheme)
          texCuspo1 = fromJust (lookup "cuspo1" imagesTheme)
          texCuspo2 = fromJust (lookup "cuspo2" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          (jogo, unlocked) = (levels state) !! (currentLevel state)

drawLevelEditor' :: State -> Picture
drawLevelEditor' state = Pictures [
    drawMap jogo texPlataforma, 
    drawSelBox state (levelEditorPos $ editorState state)]
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)

drawSelBox :: State -> Posicao -> Picture
drawSelBox state pos = uncurry Translate (posMapToGloss pos) $ (if (changingBlocks $ editorState state) then Color green else Color red) $ rectangleWire (double2Float escalaGloss) (double2Float escalaGloss)

replaceBlock :: Posicao -> Jogo -> Jogo
replaceBlock (x,y) jog = replaceMapGame (x,y) (newBlock currentBlock) jog
    where currentBlock = blocos !! floor y !! floor x
          (Mapa _ _ blocos) = mapa jog
          newBlock c = case c of
            Plataforma -> Alcapao
            Alcapao -> Escada
            Escada -> Vazio
            Vazio -> Plataforma

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

switchEnemy :: Posicao -> Jogo -> Jogo
switchEnemy pos jog = jog {
    inimigos = map (\enm -> if floorPos pos == floorPos (posicao enm) then
                    Personagem {velocidade = (0,0), 
                        tipo = case tipo enm of
                            Fantasma -> MacacoMalvado
                            MacacoMalvado -> Fantasma, 
                        emEscada = False,
                        vida = 1, 
                        pontos = 0, 
                        ressalta = True, 
                        posicao = pos, 
                        tamanho = (1,1), 
                        aplicaDano = (False, 0), 
                        direcao = Oeste,
                        temChave = False} else enm) (inimigos jog)
}

addRemoveEnemy :: Posicao -> Jogo -> Jogo
addRemoveEnemy pos jog = jog {
        inimigos = 
            if any (\enm -> (floorPos pos) == (floorPos $ posicao enm)) (inimigos jog) then
                filter (\enm -> (floorPos pos) /= (floorPos $ posicao enm)) (inimigos jog)
            else
                Personagem {velocidade = (0,0), 
                        tipo = Fantasma, 
                        emEscada = False,
                        vida = 1, 
                        pontos = 0, 
                        ressalta = True, 
                        posicao = pos, 
                        tamanho = (1,1), 
                        aplicaDano = (False, 0), 
                        direcao = Oeste,
                        temChave = False} : inimigos jog
    }
    where enmLs = zip [1..] (inimigos jog)