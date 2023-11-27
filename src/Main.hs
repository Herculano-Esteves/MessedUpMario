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
sizeWin = (round $ snd $ (snd (getMapaDimensoes escalaGloss mapaTeste)), (round $ fst $ (snd (getMapaDimensoes escalaGloss mapaTeste))))

eventHandler :: Event -> Jogo -> IO Jogo
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventHandler e jogo = return jogo

timeHandler :: Float -> Jogo -> IO Jogo
timeHandler time jogo = return $ movimenta 1 (float2Double time) jogo

-- ? Set a scale for drawng according to the size of the window
drawPlayer :: Picture -> Personagem -> Picture
drawPlayer pic jog = Color red $ Translate (((double2Float $ fst $ posicao jog) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2) ((-(double2Float $ snd $ posicao jog) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2) $ pic --rectangleSolid ((double2Float $ fst $ tamanho jog)* (double2Float escalaGloss)) ((double2Float $ snd $ tamanho jog)*double2Float escalaGloss)

drawLs :: Jogo -> Picture -> [Picture]
drawLs jogo img = map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ img) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Plataforma] (50*0.5,50*0.5) (mapa jogo))) ++
    map (\(x,y) -> Color white $ Translate ((double2Float x)-(fromIntegral $
    (fst sizeWin))/2) ((fromIntegral $ (snd sizeWin))/2 - (double2Float y)) $ Color blue $ rectangleSolid 50 50) (getcenterofhitbox escalaGloss (getMapColisions escalaGloss [Alcapao] (escalaGloss*0.5,escalaGloss*0.5) (mapa jogo)))

draw :: Jogo -> IO Picture
draw jogo = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn (show (gravidadeQuedaonoff (mapa jogo) (jogador jogo)))
    --putStrLn (show (mapa jogo))
    mario <- loadBMP "assets/mario.bmp"
    plataforma <- loadBMP "assets/Plataforma.bmp"
    return $ Pictures ([drawPlayer  mario (jogador jogo)] ++ (drawLs jogo plataforma))

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    playIO window bgColor fr jogoSamp draw eventHandler timeHandler