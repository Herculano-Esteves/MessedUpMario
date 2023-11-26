module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import GHC.Float (float2Double, double2Float)
import Tarefa2 (jogoSamp)

window :: Display
window = InWindow
    "Donkeykong"
    sizeWin --(700,700)
    (100,100)

sizeWin :: (Int, Int)
sizeWin = (floor $ fst $ (snd (getMapaDimensoes dimensaobloco mapaTeste)), floor $ snd $ (snd (getMapaDimensoes dimensaobloco mapaTeste)))

eventHandler :: Event -> Jogo -> IO Jogo
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandler e jogo = return jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return $ movimenta 1 (float2Double time) jogo

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Personagem -> Picture
drawPlayer jog = Color white $ Translate (((double2Float $ fst $ posicao jog) * double2Float dimensaobloco) - fromIntegral (fst sizeWin)/2) (((double2Float $ snd $ posicao jog) * double2Float dimensaobloco) - fromIntegral (snd sizeWin)/2) $ rectangleSolid (double2Float $ fst $ tamanho jog) (double2Float $ snd $ tamanho jog)

draw :: Jogo -> IO Picture
draw jogo = return $ Pictures [drawPlayer (jogador jogo)]

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = playIO window bgColor fr jogoSamp draw eventHandler timeHandler