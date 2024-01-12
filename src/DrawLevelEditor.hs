module DrawLevelEditor where
import Utilities
import Graphics.Gloss
import LI12324
import DrawLevel
import Tarefa1 (escalaGloss, getMapaDimensoes)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import LI12324 (Bloco(Plataforma))
import GHC.Float (double2Float)
import Mapas (jogoSamp, jog)
import Tarefa2 (floorPos, valida)
import Extras

reactLevelEditor :: Event -> State -> IO State
{-reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state = return state {
        levels = case (selectFunc $ editorState state) of
                    0 -> replace (levels state) ((currentLevel state),(replaceBlock jogo, unlocked))
                    1 -> replace (levels state) ((currentLevel state),(switchEnemy (levelEditorPos $ editorState state) jogo, unlocked))
                    2 -> replace (levels state) ((currentLevel state),(switchEnemy (levelEditorPos $ editorState state) jogo, unlocked))
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)-}
reactLevelEditor (EventKey (Char 'o') Down _ _) state = return state {
        levels = if valid then
                replace (levels state) ((currentLevel state),( tempGame $ editorState state, unlocked))
            else if (currentLevel state) == ((length $ levels state) -1) then
                init (levels state)
            else
                (levels state),
        initLevel = if valid then tempGame $ editorState state else (initLevel state),
        currentLevel = if not valid && (currentLevel state) == ((length $ levels state) -1) then
                (currentLevel state) -1 
            else
                (currentLevel state),
        editorState = (editorState state) {
            savingGame = True
        }
    }
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          valid = valida (tempGame $ editorState state)
reactLevelEditor (EventKey (Char 's') Down _ _) state = return state {
    editorState = (editorState state) {selectFunc = if ((selectFunc $ editorState state) == 3) then 0 else (selectFunc $ editorState state) + 1}}
reactLevelEditor (EventKey (Char 'n') Down _ _) state = return $ addNewLevel state
reactLevelEditor e state = return state {editorState = eventHandlerEditor e (screenSize state) (editorState state)}
          

eventHandlerEditor :: Event -> (Int, Int) -> EditorState -> EditorState
eventHandlerEditor (EventKey (SpecialKey KeyEnter) Down _ _) screen estate = estate {
    tempGame = case (selectFunc estate) of
                    0 -> replaceBlock (tempGame estate)
                    2 -> switchJogPos (tempGame estate)
}
eventHandlerEditor (EventKey (SpecialKey KeyUp) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {jogador = (jogador $ tempGame estate) {posicao = (px, py-1)}}
    }
    where (px, py) = posicao $ jogador $ tempGame estate 
eventHandlerEditor (EventKey (SpecialKey KeyDown) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {jogador = (jogador $ tempGame estate) {posicao = (px, py+1)}}
    }
    where (px, py) = posicao $ jogador $ tempGame estate
eventHandlerEditor (EventKey (SpecialKey KeyLeft) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {jogador = (jogador $ tempGame estate) {posicao = (px-1, py)}}
    }
    where (px, py) = posicao $ jogador $ tempGame estate
eventHandlerEditor (EventKey (SpecialKey KeyRight) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {jogador = (jogador $ tempGame estate) {posicao = (px+1, py)}}
    }
    where (px, py) = posicao $ jogador $ tempGame estate
eventHandlerEditor (EventKey (Char 'a') Down _ _) screen estate = estate {
        tempGame = addRemoveEnemy (tempGame estate)
    }
eventHandlerEditor (EventKey (Char 'z') Down _ _) screen estate = estate {
        tempGame = addRemoveColecs (tempGame estate)
    }
eventHandlerEditor e screen s = s

drawLevelEditor :: State -> Picture
drawLevelEditor state 
    | savingGame $ editorState state = Color red $ scale 0.2 0.2 $ if valida (tempGame $ editorState state) then Text "Saved" else Text "Not saved! Invalid map"
    | otherwise = Pictures [drawLadder jogo texEscada, drawPorta jogo texPorta, drawMap jogo texPlataforma, drawColecs state texMoeda texMartelo texChave jogo, drawAlcapao jogo texAlcapao, drawTunel jogo texTunel,
                drawEnemies state (texInimigo1,texInimigo2) texMacaco texBarril [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6] jogo,drawMorte jogo texMorte,drawSpawnPoint (editorState state), drawSelBox state, drawMapLimits (editorState state)]
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
    3 -> Color (dim cyan)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ selectorTex--rectangleWire (double2Float escalaGloss) (double2Float escalaGloss)
    where (x,y) = posicao $ jogador $ tempGame $ editorState state
          selectorTex = fromJust (lookup "selector" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))

drawSpawnPoint :: EditorState -> Picture
drawSpawnPoint estate = uncurry Translate (posMapToGlossNivel (cameraControl $ tempGame estate) pos) $ Color (dim magenta) $ circleSolid 10
    where (Mapa (pos,dir) _ _) = mapa $ tempGame estate

drawMapLimits :: EditorState -> Picture
drawMapLimits estate = Color green $ uncurry Translate (posMapToGlossNivel (cameraControl (tempGame estate)) ((fromIntegral tx/2), (fromIntegral ty/2))) $ (rectangleWire (d2f $ (fromIntegral tx)*escalaGloss) (d2f $ ((fromIntegral ty)*escalaGloss)))
    where sizeR :: (Int, Int)
          sizeR = (round $ snd (snd (getMapaDimensoes 1 (mapa jog))), round $ fst $ (snd (getMapaDimensoes 1 (mapa jog))))
          (tx, ty) = sizeR
          jog = tempGame estate
          

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
            colecionaveis = [
                (Estrela, (0.5,2.5))
            ],
            mapa = genEmptyMap (22,15),
            lostGame = 3,
            cameraControl = ((0,0),(0,0)),
            animacaoJogo = 0,
            cheatsjogo = False
          }

genEmptyMap :: (Int, Int) -> Mapa
genEmptyMap dim = Mapa ((0.5, 2.5), Oeste) (0.5, 5.5) (aux2 dim)
    where aux :: Int -> [Bloco]
          aux 0 = []
          aux x = Vazio : aux (x-1)
          aux2 :: (Int, Int) -> [[Bloco]]
          aux2 (_,0) = []
          aux2 (x,y) = aux x : aux2 (x,y-1)

addRemoveEnemy :: Jogo -> Jogo
addRemoveEnemy jog = jog {
        inimigos = 
            if any (\enm -> (floorPos pos) == (floorPos $ posicao enm)) (inimigos jog) then
                map (\enm -> if floorPos pos == floorPos (posicao enm) then
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
                        temChave = False,
                        mira= (0,0)} else enm) $
                filter (\enm -> (floorPos pos) /= (floorPos $ posicao enm) || tipo enm /= Boss) (inimigos jog)
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
                        temChave = False,
                        mira= (0,0)} : inimigos jog
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

addRemoveColecs :: Jogo -> Jogo
addRemoveColecs jog = jog {
    colecionaveis = if (any (\(col,pos) -> floorPos pos == floorPos (posicao $ jogador jog)) (colecionaveis jog)) then
            map (\(col,pos) -> if floorPos pos == floorPos (posicao $ jogador jog) then
                (case col of
                Moeda -> Martelo
                Martelo -> Chave
                Chave -> Moeda
                , pos)
                else (col,pos)) $
            filter (\(col,pos) -> floorPos pos /= floorPos (posicao $ jogador jog) || col /= Chave) (colecionaveis jog)
        else
            (Moeda, posicao $ jogador jog) : colecionaveis jog
}