module Utilities where
import Graphics.Gloss
import LI12324

type Images = [(Theme, [(String,Picture)])]

type Levels = [(Jogo, Bool)]

data State = State {
    levels :: Levels,
    currentLevel :: Int,
    menuState :: MenuState,
    currentMenu :: Menu,
    time :: Float,
    options :: Options,
    exitGame :: Bool,
    images :: Images,
    animTime :: Float,
    levelEditorPos :: Posicao
}

data Options = Options {
    currentTheme :: Theme
}

data MenuState = MenuState {
    selectedButton :: Int,
    pressingButton :: Bool
}

data Theme = Default | Minecraft deriving (Eq)
data Menu = InGame | MainMenu | OptionsMenu | LevelSelection | LevelEditor deriving (Eq)

-- Constante referente à velocidade que as personagens se movem nas escadas
ladderSpeed :: Double
ladderSpeed = 2.4

-- | Função que substitui o valor de um determinado indíce de uma lista
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs

replaceMat :: [[a]] -> (Int, Int, a) -> [[a]]
replaceMat mat (x,y,a) = replace mat (y,replace (mat !! y) (x, a))

replaceMapGame :: Posicao -> Jogo -> Jogo
replaceMapGame (x,y) jog = jog {
    mapa = (Mapa a b mat')
}
    where (Mapa a b mat) = mapa jog
          mat' = replaceMat mat (floor x,floor y,Plataforma)

-- | Retorna as posições de todosos blocos de um certo tipo num dado mapa
getPosOfBlock :: Bloco -> [[Bloco]] -> [Posicao]
getPosOfBlock bloco mat = [(x,y) | x <- [0..fromIntegral (length (head mat)-1)], y <- [0..fromIntegral (length mat)-1], mat !! round y !! round x == bloco]

getPosOfBlockMap :: Bloco -> Mapa -> [Posicao]
getPosOfBlockMap bloco (Mapa _ _ blocos) = getPosOfBlock bloco blocos