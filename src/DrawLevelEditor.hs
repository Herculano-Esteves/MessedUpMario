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
import Mapas (jogoSamp, jog)
import Tarefa2 (floorPos, valida)

reactLevelEditor :: Event -> State -> IO State
{-reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {
        levels = case (selectFunc $ editorState state) of
                    0 -> replace (levels state) ((currentLevel state),(replaceBlock jogo, unlocked))
                    1 -> replace (levels state) ((currentLevel state),(switchEnemy (levelEditorPos $ editorState state) jogo, unlocked))
                    2 -> replace (levels state) ((currentLevel state),(switchEnemy (levelEditorPos $ editorState state) jogo, unlocked))
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)-}
reactLevelEditor (EventKey (Char 'o') Down _ _) state = return state {
        levels = replace (levels state) ((currentLevel state),(if valida (tempGame $ editorState state) then tempGame $ editorState state else jogo, unlocked))
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
reactLevelEditor (EventKey (Char 's') Down _ _) state = return state {
    editorState = (editorState state) {selectFunc = if ((selectFunc $ editorState state) == 3) then 0 else (selectFunc $ editorState state) + 1}}
reactLevelEditor (EventKey (Char 'n') Down _ _) state = return $ addNewLevel state
reactLevelEditor e state = return state {editorState = eventHandlerEditor e (editorState state)}
reactLevelEditor e s = return s

eventHandlerEditor :: Event -> EditorState -> EditorState
eventHandlerEditor (EventKey (SpecialKey KeyEnter) Down _ _) mstate = mstate {
    tempGame = case (selectFunc mstate) of
                    0 -> replaceBlock (tempGame mstate)
                    1 -> switchEnemy (tempGame mstate)
                    2 -> switchJogPos (tempGame mstate)
                    3 -> switchColecs (tempGame mstate)
}
eventHandlerEditor (EventKey (SpecialKey KeyUp) Down _ _) mstate = mstate {
        tempGame = (tempGame mstate) {jogador = (jogador $ tempGame mstate) {posicao = (px, py-1)}}
    }
    where (px, py) = posicao $ jogador $ tempGame mstate 
eventHandlerEditor (EventKey (SpecialKey KeyDown) Down _ _) mstate = mstate {
        tempGame = (tempGame mstate) {jogador = (jogador $ tempGame mstate) {posicao = (px, py+1)}}
    }
    where (px, py) = posicao $ jogador $ tempGame mstate
eventHandlerEditor (EventKey (SpecialKey KeyLeft) Down _ _) mstate = mstate {
        tempGame = (tempGame mstate) {jogador = (jogador $ tempGame mstate) {posicao = (px-1, py)}}
    }
    where (px, py) = posicao $ jogador $ tempGame mstate
eventHandlerEditor (EventKey (SpecialKey KeyRight) Down _ _) mstate = mstate {
        tempGame = (tempGame mstate) {jogador = (jogador $ tempGame mstate) {posicao = (px+1, py)}}
    }
    where (px, py) = posicao $ jogador $ tempGame mstate
eventHandlerEditor (EventKey (Char 'a') Down _ _) mstate = mstate {
        tempGame = addRemoveEnemy (tempGame mstate)
    }
eventHandlerEditor e s = s

drawLevelEditor :: State -> Picture
drawLevelEditor state = Pictures [drawLadder jogo texEscada, drawPorta jogo texPorta, drawMap jogo texPlataforma, drawColecs texMoeda texMartelo texChave jogo, drawAlcapao jogo texAlcapao, drawTunel jogo texTunel,
                drawEnemies state (texCuspo1,texCuspo2) (texInimigo1,texInimigo2) texMacaco texBarril [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6] jogo,drawMorte jogo texMorte,drawSpawnPoint (editorState state), drawSelBox state]
    where texEscada = fromJust (lookup "escada" imagesTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo1 = fromJust (lookup "inimigo1" imagesTheme)
          texInimigo2 = fromJust (lookup "inimigo2" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texMartelo = fromJust (lookup "martelo" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesTheme)
          texPorta = fromJust (lookup "portaMario" imagesTheme)
          texMacaco = fromJust (lookup "macacoMalvado" imagesTheme)
          texBarril = fromJust (lookup "barril" imagesTheme)
          texMorte = fromJust (lookup "morreu" imagesTheme)
          texBoss1 = fromJust (lookup "boss1" imagesTheme)
          texBoss2 = fromJust (lookup "boss2" imagesTheme)
          texBoss3 = fromJust (lookup "boss3" imagesTheme)
          texBoss4 = fromJust (lookup "boss4" imagesTheme)
          texBoss5 = fromJust (lookup "boss5" imagesTheme)
          texBoss6 = fromJust (lookup "boss6" imagesTheme)
          texCuspo1 = fromJust (lookup "cuspo1" imagesTheme)
          texCuspo2 = fromJust (lookup "cuspo2" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          --(jogo, unlocked) = (levels state) !! (currentLevel state)
          jogo = tempGame $ editorState state

drawLevelEditor' :: State -> Picture
drawLevelEditor' state = Pictures [
    drawMap jogo texPlataforma, 
    drawSelBox state ]
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          texPlataforma = fromJust (lookup "plataforma" imagesTheme)

drawSelBox :: State -> Picture
drawSelBox state = uncurry Translate (posMapToGlossNivel (cameraControl $ tempGame $ editorState state) (x,y)) $ (case (selectFunc $ editorState state) of
    0 -> Color green
    1 -> Color red
    2 -> Color blue
    3 -> Color (dim cyan)) $ rectangleWire (double2Float escalaGloss) (double2Float escalaGloss)
    where (x,y) = posicao $ jogador $ tempGame $ editorState state

drawSpawnPoint :: EditorState -> Picture
drawSpawnPoint mstate = uncurry Translate (posMapToGlossNivel (cameraControl $ tempGame mstate) pos) $ Color (dim magenta) $ circleSolid 10
    where (Mapa (pos,dir) _ _) = mapa $ tempGame mstate

replaceBlock :: Jogo -> Jogo
replaceBlock jog = replaceMapGame (x,y) (newBlock currentBlock) jog
    where currentBlock = blocos !! floor y !! floor x
          (Mapa _ _ blocos) = mapa jog
          newBlock c = case c of
            Plataforma -> Alcapao
            Alcapao -> Escada
            Escada -> Porta
            Porta -> Tunel
            Tunel -> Vazio
            Vazio -> Plataforma
          (x,y) = posicao $ jogador jog

addNewLevel :: State -> State
addNewLevel state = state {
    levels = levels state ++ [(emptyGame, True)],
    currentLevel = length (levels state),
    editorState = (editorState state) {
        tempGame = emptyGame
    }
}
    where emptyGame = Jogo {
            jogador = jog,
            inimigos = [],
            colecionaveis = [],
            mapa = genEmptyMap (22,15),
            lostGame = False
          }

genEmptyMap :: (Int, Int) -> Mapa
genEmptyMap dim = Mapa ((0.5, 2.5), Oeste) (0.5, 5.5) (aux2 dim)
    where aux :: Int -> [Bloco]
          aux 0 = []
          aux x = Vazio : aux (x-1)
          aux2 :: (Int, Int) -> [[Bloco]]
          aux2 (_,0) = []
          aux2 (x,y) = aux x : aux2 (x,y-1)

switchEnemy ::  Jogo -> Jogo
switchEnemy jog = jog {
    inimigos = map (\enm -> if floorPos pos == floorPos (posicao enm) then
                    Personagem {velocidade = (0,0), 
                        tipo = case tipo enm of
                            Fantasma -> MacacoMalvado
                            MacacoMalvado -> Boss
                            Boss -> Fantasma, 
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
    where pos = posicao $ jogador jog

addRemoveEnemy :: Jogo -> Jogo
addRemoveEnemy jog = jog {
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
          pos = posicao $ jogador jog

switchJogPos :: Jogo -> Jogo
switchJogPos jog = jog {
    mapa = (Mapa (pos, Oeste) p1 mat),
    jogador = (jogador jog) {
        posicao = pos
    }
}
    where (Mapa (p,dir) p1 mat) = mapa jog
          pos = posicao $ jogador jog

switchColecs :: Jogo -> Jogo
switchColecs jog = jog {
    colecionaveis = if (any (\(col,pos) -> floorPos pos == floorPos (posicao $ jogador jog)) (colecionaveis jog)) then
        map (\(col, pos) -> if floorPos pos == floorPos (posicao $ jogador jog) then
            (case col of
                Moeda -> Martelo
                Martelo -> Moeda, pos)
            else (col, pos)) (colecionaveis jog)
        else (colecionaveis jog)
}