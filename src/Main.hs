module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import Mapas
import DrawMap
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
-- eventHandler (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
-- eventHandler (EventKey (SpecialKey KeyRight) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
-- eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
-- eventHandler (EventKey (SpecialKey KeyLeft) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
-- eventHandler (EventKey (SpecialKey KeyUp) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
-- eventHandler (EventKey (SpecialKey KeyUp) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
-- eventHandler (EventKey (SpecialKey KeyDown) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
-- eventHandler (EventKey (SpecialKey KeyDown) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
-- eventHandler (EventKey (SpecialKey KeySpace) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
-- --eventHandler (EventKey (SpecialKey KeySpace) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Nothing) jogo
-- eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) jogo = exitSuccess
-- eventHandler (EventKey (Char 'm') Down _ _) jogo = return $ jogo {mapa = emptyMap}
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
eventHandler (EventKey (Char 'm') Down _ _) state = return $ state {inMenu = not (inMenu state)}
eventHandler event state
    | inMenu state = return state {jogo = eventHandlerInGame event (jogo state)}
    | otherwise = eventHandlerInMenu event state

timeHandler :: Float -> State -> IO State
timeHandler time state = return $ state {jogo = movimenta 1 (float2Double time) (jogo state)}


draw :: State -> IO Picture
draw state = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador (jogo state))))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador (jogo state)) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador (jogo state)) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn ("Not on floor: " ++ show (gravidadeQuedaonoff (mapa (jogo state)) (jogador (jogo state))))
    putStrLn ("Velocidade jogador: " ++ (show (velocidade $ jogador (jogo state))))
    putStrLn ("CanGoToLeft: " ++ show (podeAndarParaEsquerdaBool (mapa (jogo state)) (jogador (jogo state))) )
    --putStrLn (show (mapa jogo))
    mario <- loadBMP "assets/mario.bmp"
    plataforma <- loadBMP "assets/Plataforma.bmp"
    escadas <- loadBMP "assets/ladder.bmp"
    if (inMenu state) then return $ Pictures ([drawLadder (jogo state) escadas, drawPlayer  mario (jogador (jogo state))] ++ (drawLs (jogo state) plataforma) ++ drawColecs (jogo state))
    else return $ Pictures [drawMenu state]

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    playIO window bgColor fr initialState draw eventHandler timeHandler