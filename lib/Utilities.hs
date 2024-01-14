module Utilities where
import Graphics.Gloss
import LI12324
import Data.Maybe (fromJust)
import GHC.Float (double2Float, float2Double)

type Images = [(Theme, [(String,Picture)])]

type Levels = [(Jogo, Bool)]

data State = State {
    levels      :: Levels,
    initLevel   :: Jogo,
    currentLevel:: Int,
    menuState :: MenuState,
    currentMenu :: Menu,
    time  :: Float,
    options :: Options,
    exitGame  :: Bool,
    images:: Images,
    animTime  :: Float,
    editorState      :: EditorState,
    cheats          :: Bool,
    screenSize      :: (Int, Int)
}

data Options = Options {
    currentTheme :: Theme,
    marioTheme :: Theme,
    platformTheme :: Theme
}

data EditorState = EditorState {
    tempGame        :: Jogo,
    levelEditorPos  :: Posicao,
    selectFunc      :: Int,
    removingEnemies :: Bool,
    savingGame      :: Bool
}

data MenuState = MenuState {
    selectedButton :: Int,
    pressingButton :: Bool
}

data Theme = Default | Minecraft | Quadradinho | Caverna deriving (Eq)
data Menu = InGame | MainMenu | OptionsMenu | LevelSelection | LevelEditor | GameOver | EndScreen deriving (Eq)

-- Constante referente à velocidade que as personagens se movem nas escadas
ladderSpeed :: Double
ladderSpeed = 2.4

-- | Função que retorna o tamanho de um bloco no gloss
escalaGloss :: Double
escalaGloss = 90

-- | Função que substitui o valor de um determinado indíce de uma lista
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

-- | Função que substitui o valor de um determinado indíce de uma matriz
replaceMat :: [[a]] -> (Int, Int, a) -> [[a]]
replaceMat mat (x,y,a) = replace mat (y,replace (mat !! y) (x, a))

replaceMapGame :: Posicao -> Bloco -> Jogo -> Jogo
replaceMapGame (x,y) bloco jog = jog {
    mapa = (Mapa a b mat')
}
    where (Mapa a b mat) = mapa jog
          mat' = replaceMat mat (floor x,floor y,bloco)

-- | Retorna as posições de todos os blocos de um certo tipo numa matriz
getPosOfBlock :: Bloco -> [[Bloco]] -> [Posicao]
getPosOfBlock bloco mat = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! round y !! round x == bloco]

-- | Retorna as posições de todos os blocos de um certo tipo num dado mapa
getPosOfBlockMap :: Bloco -> Mapa -> [Posicao]
getPosOfBlockMap bloco (Mapa _ _ blocos) = getPosOfBlock bloco blocos

mapToFile :: Mapa -> String
mapToFile (Mapa pi pf blocos) = "Mapa " ++ show pi ++ " " ++ show pf ++ " [\n" ++
    (reverse $ drop 2 $ reverse $ unlines (map (\l -> show l ++ ",") blocos)) ++ "\n]"

enemiesToFile :: [Personagem] -> String
enemiesToFile enms = unlines $ map (\e -> "Tipo: " ++ show (tipo e) ++ " Pos: " ++ show (posicao e)) enms

colecionaveisToFile :: [(Colecionavel, Posicao)] -> String
colecionaveisToFile col = "[\n" ++ (reverse $ drop 2 $ reverse (unlines $ map (\c -> show c ++ ",") col)) ++ "\n]"

drawNum :: Int -> (Float, Float) -> State -> Picture
drawNum n (x,y) state = Pictures $ (foldl (\p c -> (Translate (x + (40*(fromIntegral $ length p))) y $ scale 0.6 0.6 $
    case c of
        '1' -> um
        '2' -> dois
        '3' -> tres
        '4' -> quatro
        '5' -> cinco
        '6' -> seis
        '7' -> sete
        '8' -> oito
        '9' -> nove
        _ -> zero) : p) [] (show n))
    where um = fromJust (lookup "um" imagesTheme)
          dois = fromJust (lookup "dois" imagesTheme)
          tres = fromJust (lookup "tres" imagesTheme)
          quatro = fromJust (lookup "quatro" imagesTheme)
          cinco = fromJust (lookup "cinco" imagesTheme)
          seis = fromJust (lookup "seis" imagesTheme)
          sete = fromJust (lookup "sete" imagesTheme)
          oito = fromJust (lookup "oito" imagesTheme)
          nove = fromJust (lookup "nove" imagesTheme)
          zero = fromJust (lookup "zero" imagesTheme)
          imagesTheme = fromJust (lookup (currentTheme (options state)) (images state))


d2f = double2Float
f2d = float2Double

-- | Converte as posicoes do mapa para as posicoes do gloss
posMapToGloss :: State -> Posicao -> (Float,Float)
posMapToGloss state (x,y) = (d2f x*d2f escalaGloss-fromIntegral (fst sizeWin)/2, fromIntegral (snd sizeWin)/2 - d2f y * d2f escalaGloss)
                            where sizeWin = screenSize state