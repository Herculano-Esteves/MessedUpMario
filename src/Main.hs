module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import GHC.Float (float2Double, double2Float)
import Tarefa2 (jogoSamp)

mario :: IO Picture
mario = do
    img <- loadBMP "assets/mario.bmp"
    return img

window :: Display
window = InWindow
    "Donkeykong"
    sizeWin --(700,700)
    (100,100)

sizeWin :: (Int, Int)
sizeWin = (round $ snd $ (snd (getMapaDimensoes dimensaobloco mapaTeste)), (round $ fst $ (snd (getMapaDimensoes dimensaobloco mapaTeste))))

eventHandler :: Event -> Jogo -> IO Jogo
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandler e jogo = return jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return $ movimenta 1 (float2Double time) jogo

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Picture -> Personagem -> Picture
drawPlayer pic jog = Color red $ Translate (((double2Float $ fst $ posicao jog) * double2Float dimensaobloco) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float dimensaobloco) + fromIntegral (snd sizeWin)/2) $ pic --rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float dimensaobloco)) ((double2Float $ snd $ tamanho jog)*double2Float dimensaobloco)

drawLs :: Picture -> [Picture]
drawLs img = map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox (getMapColisions dimensaobloco [Plataforma] (dimensaobloco*0.5,dimensaobloco*0.5) mapaTeste))

draw :: Jogo -> IO Picture
draw jogo = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float dimensaobloco) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float dimensaobloco) + fromIntegral (snd sizeWin)/2))))
    putStrLn (show (gravidadeQuedaonoff (mapa jogo) (jogador jogo)))
    mario <- loadBMP "assets/mario.bmp"
    plataforma <- loadBMP "assets/Plataforma.bmp"
    return $ Pictures ([drawPlayer  mario (jogador jogo)] ++ (drawLs plataforma))

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    playIO window bgColor fr jogoSamp draw eventHandler timeHandler