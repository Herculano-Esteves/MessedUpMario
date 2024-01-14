module DrawLevelEditor where
import Utilities
import Graphics.Gloss
import LI12324
import DrawLevel
import Tarefa1 ( getMapaDimensoes)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game
import LI12324 (Bloco(Plataforma))
import GHC.Float (double2Float)
import Mapas
import Tarefa2 (floorPos, valida)
import Extras

-- | Função que recebe um estado e um evento e reage a esse evento no editor de níveis
reactLevelEditor :: Event -> State -> IO State
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
reactLevelEditor (EventKey (SpecialKey KeyEnter) Down _ _) state 
    | not $ savingGame (editorState state) = return state {
        editorState = (editorState state) {tempGame = replaceBlock (tempGame (editorState state))}
    }
    | otherwise = return state {
        currentMenu = MainMenu
    }
reactLevelEditor (EventKey (Char 'n') Down _ _) state = return $ addNewLevel state
reactLevelEditor e state = return state {editorState = eventHandlerEditor e (screenSize state) (editorState state)}
          
-- | Função que trata de todos os eventos do editor de níveis
eventHandlerEditor (EventKey (SpecialKey KeyUp) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {
            jogador = (jogador $ tempGame estate) {
                posicao = (px, if floor py > 0 then py-1 else py)
            }
        }
    }
    where (px, py) = posicao $ jogador $ tempGame estate 
eventHandlerEditor (EventKey (SpecialKey KeyDown) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {
            jogador = (jogador $ tempGame estate) {
                posicao = (px, if floor py < (length mat - 1) then py+1 else py)
            }
        }
    }
    where (px, py) = posicao $ jogador $ tempGame estate
          (Mapa (pos,dir) p1 mat) = mapa $ tempGame estate
eventHandlerEditor (EventKey (SpecialKey KeyLeft) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {
            jogador = (jogador $ tempGame estate) {
                posicao = (if floor px > 0 then px-1 else px, py)
            }
        }
    }
    where (px, py) = posicao $ jogador $ tempGame estate
eventHandlerEditor (EventKey (SpecialKey KeyRight) Down _ _) screen estate = estate {
        tempGame = cameraHitbox screen (1) $ (tempGame estate) {
            jogador = (jogador $ tempGame estate) {
                posicao = (if floor px < (length (head mat) -1) then px+1 else px, py)}}
    }
    where (px, py) = posicao $ jogador $ tempGame estate
          (Mapa (pos,dir) p1 mat) = mapa $ tempGame estate
eventHandlerEditor (EventKey (Char 'a') Down _ _) screen estate = estate {
        tempGame = addRemoveEnemy (tempGame estate)
    }

eventHandlerEditor (EventKey (Char 'z') Down _ _) screen estate = estate {
        tempGame = addRemoveColecs (tempGame estate)
    }
eventHandlerEditor (EventKey (Char '1') Down _ _) screen estate = estate {
        tempGame = switchJogPos (tempGame estate)
    }
eventHandlerEditor (EventKey (Char '2') Down _ _) screen estate = estate {
        tempGame = switchEndJogPos (tempGame estate)
    }
eventHandlerEditor (EventKey (Char 'h') Down _ _) screen estate = estate {
        showHelp = not (showHelp estate)
    }
eventHandlerEditor e screen s = s

-- | Função global que desenha tudo o que é necessário no editor de níveis
drawLevelEditor :: State -> Picture
drawLevelEditor state 
    | savingGame $ editorState state = Pictures [
        scale 1 1 $ if valida (tempGame $ editorState state) then savedText else notSavedText,
        Translate 0 (-150) $ scale 1 1 pressEnterTex
    ]
    | otherwise = Pictures [
        drawEspinho jogo texEspinho,
        drawLadder jogo texEscada,
        drawPorta jogo texPorta,
        drawMap jogo texPlataforma,
        drawColecs state texMoeda texmartelo2 texChave jogo,
        drawAlcapao jogo texAlcapao,
        drawTunel jogo texTunel,
        drawEnemies state (texInimigo1,texInimigo2) texMacaco texBarril [texBoss1,texBoss2,texBoss3,texBoss4,texBoss5,texBoss6] jogo,
        drawSpawnPoint (editorState state),
        drawSelBox state,
        drawMapLimits (editorState state),
        drawHelp state]
    where texEscada = fromJust (lookup "escada" imagesPlatformTheme)
          texPlataforma = fromJust (lookup "plataforma" imagesPlatformTheme)
          texAlcapao = fromJust (lookup "alcapao" imagesPlatformTheme)
          texTunel = fromJust (lookup "tunel" imagesTheme)
          texInimigo1 = fromJust (lookup "inimigo1" imagesTheme)
          texInimigo2 = fromJust (lookup "inimigo2" imagesTheme)
          texMoeda = fromJust (lookup "moeda" imagesTheme)
          texChave = fromJust (lookup "chavemario" imagesPlatformTheme)
          texPorta = fromJust (lookup "portaMario" imagesPlatformTheme)
          texMacaco = fromJust (lookup "macacoMalvado" imagesTheme)
          texBarril = fromJust (lookup "barril" imagesTheme)
          texMorte = fromJust (lookup "morreu" imagesTheme)
          texBoss1 = fromJust (lookup "boss1" imagesTheme)
          texBoss2 = fromJust (lookup "boss2" imagesTheme)
          texBoss3 = fromJust (lookup "boss3" imagesTheme)
          texBoss4 = fromJust (lookup "boss4" imagesTheme)
          texBoss5 = fromJust (lookup "boss5" imagesTheme)
          texBoss6 = fromJust (lookup "boss6" imagesTheme)
          texcamera = fromJust (lookup "cameraman" imagesTheme)
          texEspinho = fromJust (lookup "espinho" imagesTheme)
          martelos = [texmartelo1,texmartelo2]
          texmartelo1 = fromJust (lookup "martelo1" imagesTheme)
          texmartelo2 = fromJust (lookup "martelo2" imagesTheme)
          savedText = fromJust (lookup "savedText" imagesTheme)
          pressEnterTex = fromJust $ lookup "pressEnterText" (fromJust $ lookup Default (images state))
          notSavedText = fromJust (lookup "notSavedText" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))
          imagesPlatformTheme = fromJust (lookup (platformTheme (options state)) (images state))
          jogo = tempGame $ editorState state

-- | Função que desenha a caixa de seleção
drawSelBox :: State -> Picture
drawSelBox state = uncurry Translate (posMapToGlossNivel (cameraControl $ tempGame $ editorState state) (x,y)) $ scale (d2f escalaGloss/50) (d2f escalaGloss/50) $ selectorTex--rectangleWire (double2Float escalaGloss) (double2Float escalaGloss)
    where (x,y) = posicao $ jogador $ tempGame $ editorState state
          selectorTex = fromJust (lookup "selector" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))

-- | Função que desenha o local de spawn do jogador
drawSpawnPoint :: EditorState -> Picture
drawSpawnPoint estate = uncurry Translate (posMapToGlossNivel (cameraControl $ tempGame estate) pos) $ Color (dim magenta) $ circleSolid 10
    where (Mapa (pos,dir) _ _) = mapa $ tempGame estate

-- | Função que desenha os limites do mapa
drawMapLimits :: EditorState -> Picture
drawMapLimits estate = Color green $ uncurry Translate (posMapToGlossNivel (cameraControl (tempGame estate)) ((fromIntegral tx/2), (fromIntegral ty/2))) $ (rectangleWire (d2f $ (fromIntegral tx)*escalaGloss) (d2f $ ((fromIntegral ty)*escalaGloss)))
    where sizeR :: (Int, Int)
          sizeR = (round $ snd (snd (getMapaDimensoes 1 (mapa jog))), round $ fst $ (snd (getMapaDimensoes 1 (mapa jog))))
          (tx, ty) = sizeR
          jog = tempGame estate

drawHelp :: State -> Picture
drawHelp state = Pictures [
    if showHelp (editorState state) then scale 3 3 $ helpTex else Blank,
    Translate (-725) (-500) $ scale 0.8 0.8 helpText]
    where helpTex = fromJust (lookup "helpScreen" imagesTheme)
          helpText = fromJust (lookup "helpText" imagesTheme)
          imagesTheme = fromJust (lookup Default (images state))

-- | Função que recebe um jogo e substitui um bloco na posição do jogador
replaceBlock :: Jogo -> Jogo
replaceBlock jog = replaceMapGame (x,y) (newBlock currentBlock) jog
    where currentBlock = blocos !! floor y !! floor x
          (Mapa _ _ blocos) = mapa jog
          newBlock c = case c of
            Plataforma -> Alcapao
            Alcapao -> Escada
            Escada -> Porta
            Porta -> Tunel
            Tunel -> Espinho
            Espinho -> Vazio
            Vazio -> Plataforma
          (x,y) = posicao $ jogador jog

-- | Função que adiciona um novo nível a um dado state
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
            mapa = genEmptyMap (22,25),
            lostGame = 3,
            cameraControl = ((0,0),(0,0)),
            animacaoJogo = 0,
            cheatsjogo = False
          }

-- | Função que gera um mapa vazio com uma dada dimensão
genEmptyMap :: (Int, Int) -> Mapa
genEmptyMap dim = Mapa ((0.5, 2.5), Oeste) (0.5, 5.5) (aux2 dim)
    where aux :: Int -> [Bloco]
          aux 0 = []
          aux x = Vazio : aux (x-1)
          aux2 :: (Int, Int) -> [[Bloco]]
          aux2 (_,0) = []
          aux2 (x,y) = aux x : aux2 (x,y-1)

-- | Função que adiciona/remove ou substitui um inimigo a um jogo, na posição do jogador
addRemoveEnemy :: Jogo -> Jogo
addRemoveEnemy jog = jog {
        inimigos = 
            if any (\enm -> (floorPos pos) == (floorPos $ posicao enm)) (inimigos jog) then
                map (\enm -> if floorPos pos == floorPos (posicao enm) then
                    case tipo enm of
                        Fantasma -> macacomalvado {posicao = pos}
                        MacacoMalvado -> eyeentidade {posicao = pos}
                        EyeEntidade -> eyeboss {posicao = pos}
                        EyeBoss -> canhao {posicao = pos}
                        Canhao -> caoinimigo {posicao = pos}
                        CaoEnemy -> bowser {posicao = pos}
                        Boss -> atiradorbase {posicao = pos}
                        AtiradorBase -> fantasma {posicao = pos}
                     else enm) $
                filter (\enm -> (floorPos pos) /= (floorPos $ posicao enm) || tipo enm /= AtiradorBase) (inimigos jog)
            else
                Personagem {velocidade = (0,0),
                        tipo = Fantasma, 
                        emEscada = False,
                        vida = 1, 
                        pontos = 0, 
                        ressalta = True, 
                        posicao = pos, 
                        tamanho = (0.5,0.7), 
                        aplicaDano = (False, 0), 
                        direcao = Oeste,
                        temChave = False,
                        mira= (False,0,0)} : inimigos jog
    }
    where enmLs = zip [1..] (inimigos jog)
          pos = posicao $ jogador jog

-- | Função que altera a posição de spawn do jogador no mapa de um dado jogo, colocando-o na posição do jogador
switchJogPos :: Jogo -> Jogo
switchJogPos jog = jog {
    mapa = (Mapa (pos, Oeste) p1 mat)
}
    where (Mapa (p,dir) p1 mat) = mapa jog
          pos = posicao $ jogador jog

switchEndJogPos :: Jogo -> Jogo
switchEndJogPos jog = jog {
    mapa = (Mapa (p, dir) pos mat),
    colecionaveis = map (\(col,p) -> if col == Estrela then (col, pos) else (col,p)) (colecionaveis jog)
}
    where (Mapa (p,dir) p1 mat) = mapa jog
          pos = posicao $ jogador jog

-- | Função que adiciona/remove ou substitui colecionaveis a um dado jogo, na posição do jogador
addRemoveColecs :: Jogo -> Jogo
addRemoveColecs jog = jog {
    colecionaveis = if (any (\(col,pos) -> floorPos pos == floorPos (posicao $ jogador jog)) (colecionaveis jog)) then
            map (\(col,pos) -> if floorPos pos == floorPos (posicao $ jogador jog) then
                (case col of
                Moeda -> Martelo
                Martelo -> Chave
                Chave -> CogumeloVida
                CogumeloVida -> Moeda
                , pos)
                else (col,pos)) $
            filter (\(col,pos) -> floorPos pos /= floorPos (posicao $ jogador jog) || col /= CogumeloVida) (colecionaveis jog)
        else
            (Moeda, posicao $ jogador jog) : colecionaveis jog
}