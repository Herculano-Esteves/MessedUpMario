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

eventHandlerInGame :: Event -> Jogo -> IO Jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandlerInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandlerInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
eventHandlerInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
eventHandlerInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
eventHandlerInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
--eventHandlInGameer (EventKey (SpecialKey KeySpace) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Nothing) jogo
eventHandlerInGame (EventKey (SpecialKey KeyEsc) Down _ _) jogo = exitSuccess
eventHandlerInGame (EventKey (Char 'm') Down _ _) jogo = return $ jogo {mapa = emptyMap}
eventHandlerInGame e jogo = return jogo

eventHandler :: Event -> Jogo -> IO Jogo
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
eventHandler event jogo
    | (mapa jogo) /= emptyMap = eventHandlerInGame event jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return $ movimenta 1 (float2Double time) jogo


draw :: Jogo -> IO Picture
draw jogo = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn ("Not on floor: " ++ show (gravidadeQuedaonoff (mapa jogo) (jogador jogo)))
    putStrLn ("Velocidade jogador: " ++ (show (velocidade $ jogador jogo)))
    putStrLn ("CanGoToLeft: " ++ show (podeAndarParaEsquerdaBool (mapa jogo) (jogador jogo)) )
    --putStrLn (show (mapa jogo))
    mario <- loadBMP "assets/mario.bmp"
    plataforma <- loadBMP "assets/Plataforma.bmp"
    escadas <- loadBMP "assets/ladder.bmp"
    if (mapa jogo /= emptyMap) then return $ Pictures ([drawLadder jogo escadas, drawPlayer  mario (jogador jogo)] ++ (drawLs jogo plataforma) ++ drawColecs jogo)
    else return $ Pictures [drawTitle]

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    playIO window bgColor fr jogoSamp draw eventHandler timeHandler