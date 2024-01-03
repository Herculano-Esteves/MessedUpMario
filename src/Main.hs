module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import Mapas
import DrawLevel ( drawLevel, eventHandlerInGame, sizeWin )

import DrawMenu
import GHC.Float (float2Double, double2Float)
import System.Exit (exitSuccess)
import System.Random
import Data.Maybe (fromJust)
import Utilities
import DrawLevelEditor (drawLevelEditor, reactLevelEditor)
import Graphics.Gloss.Interface.Environment

window :: Display
window = InWindow
    "Donkeykong"
    sizeWin --(700,700)
    (300,200)



eventHandler :: Event -> State -> IO State
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
eventHandler (EventKey (Char 'm') Down _ _) state = return $ state {currentMenu = MainMenu}
eventHandler event state
    | currentMenu state == InGame = return state {levels = replace (levels state) ((currentLevel state),(eventHandlerInGame event jogo, unlocked))}
    | currentMenu state == LevelEditor = reactLevelEditor event state
    | otherwise = eventHandlerInMenu event state
    where (jogo, unlocked) = (levels state) !! (currentLevel state)

timeHandler :: Float -> State -> IO State
timeHandler dTime (State {exitGame = True}) = exitSuccess
timeHandler dTime state 
    | vida (jogador jogo) == 0 && animTime state /= 0 = if animTime state > 0 then return state {animTime = (animTime state) - dTime}
        else return state {animTime = 0}
    | lostGame jogo = return state {
            levels = replace (levels state) ((currentLevel state),((initLevel state) 
                {jogador = (jogador jogo) {posicao = pinit, direcao = dir,aplicaDano = (False,0),temChave = False}}, unlocked))
        }
    | vida (jogador jogo) == 911 = return state {
            currentLevel = (currentLevel state) + 1,
            levels = replace (levels state) (currentLevel state +1, (jogo1 {jogador = (jogador jogo1) {posicao = pinit1, direcao = dir1}}, unlckd1))
        }
    | currentMenu state == InGame = do
    generateRandomNumber <- randomRIO (1, 100 :: Int)
    return $ state {
        levels = replace (levels state) ((currentLevel state),(movimenta generateRandomNumber (float2Double dTime) jogo, unlocked)),
        time = (time state) + dTime}
    | otherwise = return state
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          (jogo1, unlckd1) = (levels state) !! (currentLevel state + 1)
          (Mapa (pinit1, dir1) _ _) = mapa jogo1
          (Mapa (pinit, dir) _ _) = mapa $ initLevel state

draw :: State -> IO Picture
draw state = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn ("Not on floor: " ++ show (gravidadeQuedaonoff (mapa (jogo)) (jogador jogo)))
    putStrLn ("Velocidade jogador: " ++ (show (velocidade $ jogador (jogo))))
    putStrLn ("Escada: " ++ show (emEscada $ jogador $ jogo))
    putStrLn ("Pontos jog: " ++ show (pontos $ jogador $ jogo))
    putStrLn ("Vida jog: " ++ show (vida $ jogador $ jogo))
    putStrLn ("Direcao jog: " ++ show (direcao $ jogador $ jogo))
    putStrLn ("Pressing button: " ++ show (pressingButton $ menuState state))
    putStrLn("velocidade enm: " ++ show (map velocidade (inimigos jogo)))

    --putStrLn (show (mapa jogo))
    if (currentMenu state == InGame) then return (drawLevel state)
    else if (currentMenu state == LevelEditor) then return (drawLevelEditor state)
    
    else return (drawMenu state)
    where (jogo, unlocked) = (levels state) !! (currentLevel state)

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

loadImages :: State -> IO State
loadImages state = do
    -- Start of Default theme
    marioParado <- loadBMP "assets/MarioTexture/MarioParado.bmp"
    marioAndar1 <- loadBMP "assets/MarioTexture/MarioAni1.bmp"
    marioAndar2 <- loadBMP "assets/MarioTexture/MarioAni2.bmp"
    mariosaltar <- loadBMP "assets/MarioTexture/Mariosaltar.bmp"
    plataforma <- loadBMP "assets/MarioTexture/Plataforma.bmp"
    escada <- loadBMP "assets/MarioTexture/ladder.bmp"
    alcapao <- loadBMP "assets/MarioTexture/Alcapao.bmp"
    tunel <- loadBMP "assets/MarioTexture/Tunel.bmp"
    inimigo <- loadBMP "assets/MarioTexture/Inimigo.bmp"
    moeda <- loadBMP "assets/MarioTexture/Moeda.bmp"
    martelo <- loadBMP "assets/MarioTexture/Martelo.bmp"
    mariocair <- loadBMP "assets/MarioTexture/Mariocair.bmp"
    chavemario <- loadBMP "assets/MarioTexture/Key.bmp"
    portamario <- loadBMP "assets/MarioTexture/Porta.bmp"
    macacomalvado <- loadBMP "assets/MarioTexture/MacacoMalvado.bmp"
    barrilmario <- loadBMP "assets/MarioTexture/Barril.bmp"
    boss1mario <- loadBMP "assets/MarioTexture/Boss1.bmp"
    mortemario <- loadBMP "assets/Death.bmp"
    
    -- Start of Minecraft theme
    relva <- loadBMP "assets/MinecraftTexture/relva.bmp"
    moedaminecraft <- loadBMP "assets/MinecraftTexture/Moedaminecraft.bmp"
    steveandar <- loadBMP "assets/MinecraftTexture/Steveandar.bmp"
    steveandar1 <- loadBMP "assets/MinecraftTexture/Steveandar.bmp"
    steveandar2 <- loadBMP "assets/MinecraftTexture/Steveandar.bmp"
    stevesaltar <- loadBMP "assets/MinecraftTexture/Stevesaltar.bmp"
    stevecair <- loadBMP "assets/MinecraftTexture/Stevecair.bmp"
    inimigominecraft <- loadBMP "assets/MinecraftTexture/Inimigominecraft.bmp"
    alcapaominecraft <- loadBMP "assets/MinecraftTexture/AlcapaoMinecraft.bmp"
    espadaminecraft <- loadBMP "assets/MinecraftTexture/EspadaMinecraft.bmp"
    escadaminecraft <- loadBMP "assets/MinecraftTexture/Escada.bmp"
    portaminecraft <- loadBMP "assets/MinecraftTexture/Porta.bmp"
    keyminecraft <- loadBMP "assets/MinecraftTexture/Key.bmp"
    macacomalvado <- loadBMP "assets/MarioTexture/MacacoMalvado.bmp"
    barrilmario <- loadBMP "assets/MarioTexture/Barril.bmp"
    --Start of buttons
    botaostart <- loadBMP "assets/Buttons/BotaoStart.bmp"
    botaostartHover <- loadBMP "assets/Buttons/BotaoStartHover.bmp"
    botaostartPressed <- loadBMP "assets/Buttons/BotaoStartPressed.bmp"
    botaoSettings <- loadBMP "assets/Buttons/BotaoSettings.bmp"
    botaoSettingsHover <- loadBMP "assets/Buttons/BotaoSettingsHover.bmp"
    botaoSettingsPressed <- loadBMP "assets/Buttons/BotaoSettingsPressed.bmp"
    menuBanner <- loadBMP "assets/MenuPrototype.bmp"
    botaoQuit <- loadBMP "assets/Buttons/BotaoQuit.bmp"
    botaoQuitHover <- loadBMP "assets/Buttons/BotaoQuitHover.bmp"
    botaoQuitPressed <- loadBMP "assets/Buttons/BotaoQuitPressed.bmp"
    return  state {
        images = [
            (Default,
            [("marioParado", marioParado),
            ("marioAndar1", marioAndar1),
            ("marioAndar2", marioAndar2),
            ("mariosaltar", mariosaltar),
            ("escada", escada),
            ("plataforma", plataforma),
            ("alcapao", alcapao),
            ("tunel", tunel),
            ("inimigo", inimigo),
            ("moeda", moeda),
            ("martelo", martelo),
            ("mariocair", mariocair),
            ("chavemario", chavemario),
            ("botaostart", botaostart),
            ("botaostartHover", botaostartHover),
            ("botaostartPressed", botaostartPressed),
            ("botaoSettings", botaoSettings),
            ("botaoSettingsHover", botaoSettingsHover),
            ("botaoSettingsPressed", botaoSettingsPressed),
            ("botaoQuit", botaoQuit),
            ("botaoQuitHover", botaoQuitHover),
            ("botaoQuitPressed", botaoQuitPressed),
            ("menuBanner", menuBanner),
            ("portaMario",portamario),
            ("macacoMalvado", macacomalvado),
            ("barril",barrilmario),
            ("morreu",mortemario),
            ("boss1", boss1mario)]),
            (Minecraft,
            [("marioParado", steveandar),
            ("marioAndar1", steveandar1),
            ("marioAndar2", steveandar2),
            ("mariosaltar", stevesaltar),
            ("escada", escadaminecraft),
            ("plataforma", relva),
            ("alcapao", alcapaominecraft),
            ("tunel", tunel),
            ("inimigo", inimigominecraft),
            ("moeda", moedaminecraft),
            ("martelo", espadaminecraft),
            ("mariocair", stevecair),
            ("chavemario", keyminecraft),
            ("botaostart", botaostart),
            ("portaMario",portaminecraft),
            ("macacoMalvado", macacomalvado),
            ("barril",barrilmario),
            ("morreu",mortemario)])
            ]
        }


main :: IO ()
main = do
    putStrLn (show (fst sizeWin, snd sizeWin))
    initState <- loadImages initialState
    playIO window bgColor fr initState draw eventHandler timeHandler