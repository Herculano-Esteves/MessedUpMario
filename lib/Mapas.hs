module Mapas where

import LI12324
import Tarefa2 (jogoSamp)
import Graphics.Gloss

emptyMap :: Mapa
emptyMap = Mapa ((0,0),Norte) (0,0) [[]]

type Images = [(String,Picture)]

data State = State {
    jogo :: Jogo,
    currentMenu :: Menu,
    selectedButton :: Int,
    exitGame :: Bool,
    images :: Images
}

data Menu = InGame | MainMenu deriving (Eq)

initialState :: State
initialState = State {
    jogo = jogoSamp,
    currentMenu = MainMenu,
    selectedButton = 0,
    exitGame = False,
    images = []
}