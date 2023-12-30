module Mapas where

import LI12324
--import Tarefa2 (jogoSamp, jog, colec, inm)
import Graphics.Gloss
import Utilities


initialState :: State
initialState = State {
    levels = [
        jogoSamp,
        jogo1,
        jogo2
        ],
    currentLevel = 0,
    currentMenu = MainMenu,
    time = 0,
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

data Letra
    = P
    | E
    | A
    | V
    | T
    | O


mapaTradutor :: [[Letra]] -> [[Bloco]]
mapaTradutor lista = map (map trocaLetras) lista

trocaLetras :: Letra -> Bloco
trocaLetras a = case a of
                E -> Escada
                P -> Plataforma
                A -> Alcapao
                V -> Vazio
                T -> Tunel
                O -> Porta

mapaDoBoss =    [
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P],
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,A,A,A,P],
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,V,V,V,V,O,V,V,V,E,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V],
            [P,P,P,P,P,P,P,P,P,P,P,P,P,A,A,A,P,P,P,P,P,P],
            [V,V,V,V,V,V,V,V,P,V,V,V,P,V,V,V,V,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,P,V,V,V,P,V,V,V,V,V,V,V,V,V],
            [V,V,V,V,V,V,V,V,P,V,V,V,P,V,V,V,V,V,V,V,V,V],
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,V,V,P,P],
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],
            [V,E,V,V,P,P,P,P,P,P,P,P,P,P,P,P,V,E,V,V,P,P],
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V],
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V],
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V],
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P]
                ]


mapaTeste = Mapa ((0.5, 2.5), Oeste) (0.5, 5.5)
    [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Alcapao, Alcapao, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio]
    ,[Vazio, Porta, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ]

inm :: [Personagem]
inm = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (13.5,13.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (7.5,5.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (9.5,9.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False},
        Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,1.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (-5,0), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False}
                    ]
jog :: Personagem
jog = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (6.0,5.5), 
                    tamanho = (0.9,1.1), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False}


-- TESTE DATA END

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda,(2.5,13.5)),(Martelo,(7.5,9.5)),(Chave,(4.5,13.5)),(Estrela,(2.5,5.5))]

jogoSamp ::Jogo
jogoSamp = Jogo mapaTeste inm colec jog

emptyMap :: Mapa
emptyMap = Mapa ((0,0),Norte) (0,0) [[]]

mapa1 = Mapa ((0.5, 2.5), Oeste) (0.5, 2.5)
    [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Alcapao, Alcapao, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Vazio, Escada, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Tunel, Vazio, Vazio, Vazio, Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ]

jogo1 ::Jogo
jogo1 = Jogo mapa1 inm colec jog



--Inicio Jogo2
jogo2 :: Jogo
jogo2 = Jogo (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)) inm colec jog