module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import LI12324
import Tarefa1
import Tarefa3
import Tarefa4
import Mapas
import DrawLevel ( drawLevel, eventHandlerInGame )
import Extras

import DrawMenu
import GHC.Float (float2Double, double2Float)
import System.Exit (exitSuccess)
import System.Random
import Data.Maybe (fromJust)
import Utilities
import DrawLevelEditor (drawLevelEditor, reactLevelEditor)
import Graphics.Gloss.Interface.Environment


-- window :: Display
-- window = InWindow
    -- "Donkeykong"
    -- sizeWin --(700,700)
    -- (300,200)
window :: Display
window = FullScreen



eventHandler :: Event -> State -> IO State
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) state = exitSuccess
eventHandler (EventKey (Char 'm') Down _ _) state = return $ state {currentMenu = MainMenu,levels = replace (levels state) (currentLevel state,(jogo {lostGame = 4}, unlocked))}
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
eventHandler (EventKey (Char 'u') Down _ _) state = do
    writeFile "game.txt" (show (tempGame $ editorState state))
    return $ state
eventHandler (EventKey (Char 'l') Down _ _) state = do
    writeFile "gameDebug.txt" (mapToFile (mapa (tempGame $ editorState state)))
    return $ state
eventHandler (EventKey (Char 'y') Down _ _) state = do
    gameFile <-readFile "game.txt"
    return $ state {
        editorState = (editorState state) {
            tempGame = read gameFile
        }}
eventHandler (EventKey (Char 'p') Down _ _) state = return $ state {levels = replace (levels state) (currentLevel state,(jogo {lostGame = if lostGame jogo == 3 then 5 else 3}, unlocked))}
    where (jogo, unlocked) = (levels state) !! (currentLevel state)

eventHandler (EventKey (Char 'c') Down _ _) state = return $ state {cheats = not (cheats state)}
eventHandler event state
    | currentMenu state == InGame = return state {levels = replace (levels state) (currentLevel state,(eventHandlerInGame event jogo, unlocked))}
    | currentMenu state == LevelEditor = reactLevelEditor event state
    | otherwise = eventHandlerInMenu event state
    where (jogo, unlocked) = (levels state) !! (currentLevel state)

timeHandler :: Float -> State -> IO State
timeHandler dTime (State {exitGame = True}) = exitSuccess
timeHandler dTime state
    -- | vida (jogador jogo) == 0 && animTime state /= 0 = if animTime state > 0 then return state {animTime = (animTime state) - dTime}
        -- else return state {animTime = 0}
    | (currentLevel state) == length (levels state) - 1 && lostGame jogo == 1 = return $ state {
        currentMenu = MainMenu,
        levels = replace (levels state) ((currentLevel state),(initLevel state, True))
    }
    | lostGame jogo == 0 = return state {
        currentMenu = GameOver}
    | lostGame jogo == 4 = return state {
            levels = replace (levels state) ((currentLevel state),((initLevel state)
                {jogador = (jogador jogo) {posicao = pinit, direcao = dir,aplicaDano = (False,0),temChave = False}}, unlocked))
        }
    | lostGame jogo == 1 = return state {
            initLevel = jogo1,
            currentLevel = (currentLevel state) + 1,
            levels = replace restoredLevels (currentLevel state +1, (jogo1 {jogador = (jogador jogo1) {posicao = pinit1, direcao = dir1}}, unlckd1))
        }
    | currentMenu state == InGame = do
    generateRandomNumber <- randomRIO (1, 100 :: Int)
    return $ extrasFuncao generateRandomNumber (float2Double dTime) state {
        levels = replace (levels state) ((currentLevel state),(movimenta generateRandomNumber (float2Double dTime) jogo, unlocked)),
        time = (time state) + dTime}
    | otherwise = return state
    where (jogo, unlocked) = (levels state) !! (currentLevel state)
          (jogo1, unlckd1) = (levels state) !! (currentLevel state + 1)
          (Mapa (pinit1, dir1) _ _) = mapa jogo1
          (Mapa (pinit, dir) _ _) = mapa $ initLevel state
          restoredLevels = replace (levels state) ((currentLevel state), (initLevel state, True))

draw :: State -> IO Picture
draw state = do
    putStrLn ("Posicao jog: " ++ (show (posicao $ jogador jogo)))
    -- putStrLn ("Posicao jog scaled: " ++ (show ((((double2Float $ fst $ posicao $ jogador jogo) * double2Float escalaGloss) - fromIntegral (fst sizeWin)/2), ((-(double2Float $ snd $ posicao $ jogador jogo) * double2Float escalaGloss) + fromIntegral (snd sizeWin)/2))))
    putStrLn ("Not on floor: " ++ show (gravidadeQuedaonoff (mapa (jogo)) (jogador jogo)))
    putStrLn ("Velocidade jogador: " ++ (show (velocidade $ jogador (jogo))))
    putStrLn ("Escada: " ++ show (emEscada $ jogador $ jogo))
    putStrLn ("Pontos jog: " ++ show (pontos $ jogador $ jogo))
    putStrLn ("Vida jog: " ++ show (vida $ jogador $ jogo))
    putStrLn ("Direcao jog: " ++ show (direcao $ jogador $ jogo))
    putStrLn ("Pressing button: " ++ show (pressingButton $ menuState state))
    putStrLn ("velocidade enm: " ++ show (map velocidade (inimigos jogo)))
    putStrLn ("selected Level: " ++ show (currentLevel state))
    putStrLn ("length Level: " ++ show (length $ levels state))

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

    plataforma <- loadBMP "assets/MarioTexture/Plataforma.bmp"
    escada <- loadBMP "assets/MarioTexture/ladder.bmp"
    alcapao <- loadBMP "assets/MarioTexture/Alcapao.bmp"
    tunel <- loadBMP "assets/MarioTexture/Tunel.bmp"
    cameraman <- loadBMP "assets/MarioTexture/CameraMan.bmp"
    inimigo1 <- loadBMP "assets/MarioTexture/Fantasma1.bmp"
    inimigo2 <- loadBMP "assets/MarioTexture/Fantasma2.bmp"
    moeda <- loadBMP "assets/MarioTexture/Moeda.bmp"
    martelo1 <- loadBMP "assets/MarioTexture/Martelo1.bmp"
    martelo2 <- loadBMP "assets/MarioTexture/Martelo2.bmp"
    chavemario <- loadBMP "assets/MarioTexture/Key.bmp"
    portamario <- loadBMP "assets/MarioTexture/Porta.bmp"
    macacomalvado <- loadBMP "assets/MarioTexture/MacacoMalvado.bmp"
    barrilmario <- loadBMP "assets/MarioTexture/Barril.bmp"
    espinho <- loadBMP "assets/MarioTexture/Espinho.bmp"
    mortemario <- loadBMP "assets/Death.bmp"
    --MARIO ANIMAÇOES
        --MARIO CORRER
    mariocorrer1 <- loadBMP "assets/MarioAnimations/MarioAndar/Andar1.bmp"
    mariocorrer2 <- loadBMP "assets/MarioAnimations/MarioAndar/Andar2.bmp"
    mariocorrer3 <- loadBMP "assets/MarioAnimations/MarioAndar/Andar3.bmp"
    marioolhos <- loadBMP "assets/MarioAnimations/MarioAndar/Olhos.bmp"
    escada1 <- loadBMP "assets/MarioAnimations/MarioEscadas/Escadas1.bmp"
    escada2 <- loadBMP "assets/MarioAnimations/MarioEscadas/Escadas2.bmp"
    morreumario <- loadBMP "assets/MarioAnimations/MorteMario.bmp"
    mariocair <- loadBMP "assets/MarioAnimations/MarioAr/MarioCair1.bmp"
    mariosaltar1 <- loadBMP "assets/MarioAnimations/MarioAr/MarioSaltar1.bmp"
    mariosaltar2 <- loadBMP "assets/MarioAnimations/MarioAr/MarioSaltar2.bmp"
    mariomartelo1 <- loadBMP "assets/MarioAnimations/MarioAndar/AndarMartelo1.bmp"
    mariomartelo2 <- loadBMP "assets/MarioAnimations/MarioAndar/AndarMartelo2.bmp"
    mariomartelo3 <- loadBMP "assets/MarioAnimations/MarioAndar/AndarMartelo3.bmp"
    marioParado <- loadBMP "assets/MarioTexture/MarioParado.bmp"
    marioAndar1 <- loadBMP "assets/MarioTexture/MarioAni1.bmp"
    marioAndar2 <- loadBMP "assets/MarioTexture/MarioAni2.bmp"
    --Boss Mario
    boss1mario <- loadBMP "assets/Bosses/Boss1.bmp"
    boss2mario <- loadBMP "assets/Bosses/Boss2.bmp"
    boss3mario <- loadBMP "assets/Bosses/Boss3.bmp"
    boss4mario <- loadBMP "assets/Bosses/Boss4.bmp"
    boss5mario <- loadBMP "assets/Bosses/Boss5.bmp"
    boss6mario <- loadBMP "assets/Bosses/Boss6.bmp"
    cuspo1 <- loadBMP "assets/MarioTexture/CuspoFogo1.bmp"
    cuspo2 <- loadBMP "assets/MarioTexture/CuspoFogo2.bmp"
    cuspo3 <- loadBMP "assets/MarioTexture/CuspoFogo3.bmp"
    ataque1 <- loadBMP "assets/Bosses/ataque/ataque1.bmp"
    ataque2 <- loadBMP "assets/Bosses/ataque/ataque2.bmp"
    ataque3 <- loadBMP "assets/Bosses/ataque/ataque3.bmp"
    ataque4 <- loadBMP "assets/Bosses/ataque/ataque4.bmp"
    ataque5 <- loadBMP "assets/Bosses/ataque/ataque5.bmp"
    ataque6 <- loadBMP "assets/Bosses/ataque/ataque6.bmp"
    ataque7 <- loadBMP "assets/Bosses/ataque/ataque7.bmp"
    ataque8 <- loadBMP "assets/Bosses/ataque/ataque8.bmp"
    ataque9 <- loadBMP "assets/Bosses/ataque/ataque9.bmp"
    ataque10 <- loadBMP "assets/Bosses/ataque/ataque10.bmp"
    --EYE BOSS
    olhobranco <- loadBMP "assets/Bosses/EyeBoss/EyeWhite.bmp"
    olhoazul <- loadBMP "assets/Bosses/EyeBoss/EyeBlue.bmp"
    -- Estrela mario
    estrela1 <- loadBMP "assets/Estrela/Estrela1.bmp"
    estrela2 <- loadBMP "assets/Estrela/Estrela2.bmp"
    estrela3 <- loadBMP "assets/Estrela/Estrela3.bmp"
    estrela4 <- loadBMP "assets/Estrela/Estrela4.bmp"
    estrela5 <- loadBMP "assets/Estrela/Estrela5.bmp"
    estrela6 <- loadBMP "assets/Estrela/Estrela6.bmp"
    estrela7 <- loadBMP "assets/Estrela/Estrela7.bmp"
    estrela8 <- loadBMP "assets/Estrela/Estrela8.bmp"
    estrela9 <- loadBMP "assets/Estrela/Estrela9.bmp"
    estrela10 <- loadBMP "assets/Estrela/Estrela10.bmp"
    estrela11 <- loadBMP "assets/Estrela/Estrela11.bmp"
    estrela12 <- loadBMP "assets/Estrela/Estrela12.bmp"


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
    arrow <- loadBMP "assets/Buttons/arrow.bmp"
    -- Numbers
    um <- loadBMP "assets/Numbers/Um.bmp"
    dois <- loadBMP "assets/Numbers/Dois.bmp"
    tres <- loadBMP "assets/Numbers/Tres.bmp"
    quatro <- loadBMP "assets/Numbers/Quatro.bmp"
    cinco <- loadBMP "assets/Numbers/Cinco.bmp"
    seis <- loadBMP "assets/Numbers/Seis.bmp"
    sete <- loadBMP "assets/Numbers/Sete.bmp"
    oito <- loadBMP "assets/Numbers/Oito.bmp"
    nove <- loadBMP "assets/Numbers/Nove.bmp"
    zero <- loadBMP "assets/Numbers/Zero.bmp"
    -- Backgrounds
    bgMenu <- loadBMP "assets/Backgrounds/menubackgrounds.bmp"
    -- Level editor
    selector <- loadBMP "assets/NoAplication/selector.bmp"
    return  state {
        images = [
            (Default,
            [("marioParado", marioParado),
            ("marioAndar1", marioAndar1),
            ("marioAndar2", marioAndar2),
            ("mariosaltar1", mariosaltar1),
            ("mariosaltar2", mariosaltar2),
            ("mariomartelo1", mariomartelo1),
            ("mariomartelo2", mariomartelo2),
            ("mariomartelo3", mariomartelo3),
            ("escada", escada),
            ("plataforma", plataforma),
            ("alcapao", alcapao),
            ("tunel", tunel),
            ("inimigo1", inimigo1),
            ("inimigo2", inimigo2),
            ("moeda", moeda),
            ("martelo1", martelo1),
            ("martelo2", martelo2),
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
            ("arrow", arrow),
            ("menuBanner", menuBanner),
            ("portaMario",portamario),
            ("macacoMalvado", macacomalvado),
            ("barril",barrilmario),
            ("morreu",mortemario),
            ("cameraman", cameraman),
            ("espinho",espinho),
            --ANIMATIONS MARIO
            ("marioandar1",mariocorrer1),
            ("marioandar2",mariocorrer2),
            ("marioandar3",mariocorrer3),
            ("marioolhos",marioolhos),
            ("escada1",escada1),
            ("escada2",escada2),
            ("morreumario",morreumario),
            -- Boss Mario
            ("boss1", boss1mario),
            ("boss2", boss2mario),
            ("boss3", boss3mario),
            ("boss4", boss4mario),
            ("boss5", boss5mario),
            ("boss6", boss6mario),
            ("cuspo1",cuspo1),
            ("cuspo2",cuspo2),
            ("cuspo3",cuspo3),
            ("ataqueboss1",ataque1),
            ("ataqueboss2",ataque2),
            ("ataqueboss3",ataque3),
            ("ataqueboss4",ataque4),
            ("ataqueboss5",ataque5),
            ("ataqueboss6",ataque6),
            ("ataqueboss7",ataque7),
            ("ataqueboss8",ataque8),
            ("ataqueboss9",ataque9),
            ("ataqueboss10",ataque10),
            --Boss EYE
            ("olhobranco",olhobranco),
            ("olhoazul",olhoazul),
            -- Estrela Mario
            ("estrela1",estrela1),
            ("estrela2",estrela2),
            ("estrela3",estrela3),
            ("estrela4",estrela4),
            ("estrela5",estrela5),
            ("estrela6",estrela6),
            ("estrela7",estrela7),
            ("estrela8",estrela8),
            ("estrela9",estrela9),
            ("estrela10",estrela10),
            ("estrela11",estrela11),
            ("estrela12",estrela12),
            -- numeros
            ("um", um),
            ("dois", dois),
            ("tres", tres),
            ("quatro", quatro),
            ("cinco", cinco),
            ("seis", seis),
            ("sete", sete),
            ("oito", oito),
            ("nove", nove),
            ("zero", zero),
            -- Backgrounds
            ("bgMenu", bgMenu),
            -- Level Editor
            ("selector", selector)
            ]),
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
    scrSize <- getScreenSize
    initState <- loadImages initialState {
        screenSize = scrSize
    }
    playIO window bgColor fr initState draw eventHandler timeHandler