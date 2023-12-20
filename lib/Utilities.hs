module Utilities where
import Graphics.Gloss
import LI12324

type Images = [(Theme, [(String,Picture)])]

type Levels = [Jogo]

data State = State {
    levels :: Levels,
    currentLevel :: Int,
    menuState :: MenuState,
    currentMenu :: Menu,
    options :: Options,
    exitGame :: Bool,
    images :: Images
}

data Options = Options {
    currentTheme :: Theme
}

data MenuState = MenuState {
    selectedButton :: Int,
    pressingButton :: Bool
}

data Theme = Default | Minecraft deriving (Eq)
data Menu = InGame | MainMenu | OptionsMenu deriving (Eq)

-- | Função que substitui o valor de um determinado indíce de uma lista
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) = before ++ [e] ++ after
  where
    (before, _:after) = splitAt i xs