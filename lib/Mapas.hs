module Mapas where

import LI12324
import Tarefa2 (jogoSamp)
import Graphics.Gloss

emptyMap :: Mapa
emptyMap = Mapa ((0,0),Norte) (0,0) [[]]

type Images = [(Theme, [(String,Picture)])]

data State = State {
    jogo :: Jogo,
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


initialState :: State
initialState = State {
    jogo = jogoSamp,
    currentMenu = MainMenu,
    options = Options {
        currentTheme = Default
        },
    menuState = MenuState {
        selectedButton = 0,
        pressingButton = False
    },
    exitGame = False,
    images = []
}