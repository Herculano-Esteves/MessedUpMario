module Mapas where

import LI12324
import Tarefa2 (jogoSamp)

emptyMap :: Mapa
emptyMap = Mapa ((0,0),Norte) (0,0) [[]]

data State = State {
    jogo :: Jogo,
    inMenu :: Bool,
    selectedButton :: Int 
}

initialState :: State
initialState = State {
    jogo = jogoSamp,
    inMenu = True,
    selectedButton = 0
}