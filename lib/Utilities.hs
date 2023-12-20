module Utilities where
import Graphics.Gloss
import LI12324

type Images = [(Theme, [(String,Picture)])]

type Levels = [(Int, Jogo)]

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