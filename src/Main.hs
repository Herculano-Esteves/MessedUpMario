module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import Mapas
import DrawLevel
import DrawMenu
import GHC.Float (float2Double, double2Float)
import Tarefa2 (jogoSamp)
import System.Exit (exitSuccess)

window :: Display
window = InWindow
    "Donkeykong"
    sizeWin --(700,700)
    (700,200)

eventHandler :: Event -> State -> IO State
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
eventHandler (EventKey (Char 'm') Down _ _) state = return $ state {currentMenu = MainMenu}
eventHandler event state
    | currentMenu state == InGame = return state {jogo = eventHandlerInGame event (jogo state)}
    | otherwise = eventHandlerInMenu event state

timeHandler :: Float -> State -> IO State
timeHandler time (State {exitGame = True}) = exitSuccess
timeHandler time state = return $ state {jogo = movimenta 1 (float2Double time) (jogo state)}

draw :: State -> IO Picture
draw state = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador (jogo state))))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador (jogo state)) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador (jogo state)) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn ("Not on floor: " ++ show (gravidadeQuedaonoff (mapa (jogo state)) (jogador (jogo state))))
    putStrLn ("Velocidade jogador: " ++ (show (velocidade $ jogador (jogo state))))
    putStrLn ("CanGoToLeft: " ++ show (podeAndarParaEsquerdaBool (mapa (jogo state)) (jogador (jogo state))) )
    --putStrLn (show (mapa jogo))
    if (currentMenu state == InGame) then return (drawLevel state)
    else return (drawMenu state)

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

loadImages :: State -> IO State
loadImages state = do
    marioandar <- loadBMP "assets/Marioandar.bmp"
    mariosaltar <- loadBMP "assets/Mariosaltar.bmp"
    plataforma <- loadBMP "assets/Plataforma.bmp"
    escada <- loadBMP "assets/ladder.bmp"
    alcapao <- loadBMP "assets/Alcapao.bmp"
    tunel <- loadBMP "assets/Tunel.bmp"
    inimigo <- loadBMP "assets/Inimigo.bmp"
    moeda <- loadBMP "assets/Moeda.bmp"
    martelo <- loadBMP "assets/Martelo.bmp"
    mariocair <- loadBMP "assets/Mariocair.bmp"
    return  state {
        images = [
            ("marioandar", marioandar),
            ("mariosaltar", mariosaltar),
            ("escada", escada),
            ("plataforma", plataforma),
            ("alcapao", alcapao),
            ("tunel", tunel),
            ("inimigo", inimigo),
            ("moeda", moeda),
            ("martelo", martelo),
            ("mariocair", mariocair)
            ]
        }

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    initState <- loadImages initialState
    playIO window bgColor fr initState draw eventHandler timeHandler