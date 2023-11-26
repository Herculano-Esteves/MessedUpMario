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
    (700,700)
    (100,100)

eventHandler :: Event -> Jogo -> IO Jogo
eventHandler event jogo = return jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return jogo

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Personagem -> Picture
drawPlayer jog = Color white $ Translate (double2Float $ fst $ posicao jog) (double2Float $ snd $ posicao jog) $ rectangleSolid (double2Float $ fst $ tamanho jog) (double2Float $ snd $ tamanho jog)

draw :: Jogo -> IO Picture
draw jogo = return $ Pictures [drawPlayer (jogador jogo)]

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = playIO window bgColor fr jogoSamp draw eventHandler timeHandler