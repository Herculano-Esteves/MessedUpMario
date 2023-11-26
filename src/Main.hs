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
sizeWin = (round $ fst $ (snd (getMapaDimensoes dimensaobloco mapaTeste)), round $ snd $ (snd (getMapaDimensoes dimensaobloco mapaTeste)))

eventHandler :: Event -> Jogo -> IO Jogo
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandler e jogo = return jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return $ movimenta 1 (float2Double time) jogo

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Personagem -> Picture
drawPlayer jog = Color white $ Translate (((double2Float $ fst $ posicao jog) * double2Float dimensaobloco) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float dimensaobloco) + fromIntegral (snd sizeWin)/2) $ rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float dimensaobloco)) ((double2Float $ snd $ tamanho jog)*double2Float dimensaobloco)

drawLs :: [Picture]
drawLs = map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $ (fst sizeWin))/2) ((double2Float y)-(fromIntegral $ (snd sizeWin))/2) $ (rectangleSolid 50 50)) (getcenterofhitbox (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapaTeste))

draw1 :: Picture
draw1 = Color red $ rectangleSolid 20 20
draw11 = Translate (-12.5) 12.5 $ Color green $ rectangleSolid 5 5

draw :: Jogo -> IO Picture
draw jogo = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float dimensaobloco) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float dimensaobloco) + fromIntegral (snd sizeWin)/2))))
    return $ Pictures ([drawPlayer (jogador jogo), draw1, draw11] ++ drawLs)

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    playIO window bgColor fr jogoSamp draw eventHandler timeHandler