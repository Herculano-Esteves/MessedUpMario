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
                    vida = 3, 
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
jog2 :: Personagem
jog2 = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (3.0,3.5), 
                    tamanho = (0.9,1.1), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False}
inmjogo2 :: [Personagem]
inmjogo2 = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (7.5,15.5), 
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
                    posicao = (3.5,19.5), 
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
                    posicao = (20.5,7.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False}
                    ]

colecjogo2 :: [(Colecionavel, Posicao)]
colecjogo2 = [(Moeda,(2.5,17.5)),(Martelo,(7.5,15.5)),(Chave,(4.5,11.5)),(Estrela,(2.5,7.5))]

mapaDoBoss :: [[Letra]]
mapaDoBoss =    [
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P],--1
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--2
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--3
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--4
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,A,A,A,P],--5
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V],--6
            [V,V,V,V,V,V,V,V,V,V,V,V,O,V,V,V,E,V,V,V,V,V],--7
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V],--8
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,A,A,P,P,P,P,P,P],--9
            [V,V,V,V,V,V,V,V,P,V,V,V,P,E,V,V,V,V,V,V,V,V],--10
            [V,V,V,V,V,V,V,V,P,V,V,V,P,E,V,V,V,V,V,V,V,V],--11
            [V,V,V,V,V,V,V,V,P,V,V,V,P,E,V,V,V,V,V,V,V,V],--12
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,V,V,P,P],--13
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],--14
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],--15
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V],--16
            [V,E,V,V,P,P,P,P,P,P,P,P,P,P,P,P,V,E,V,V,P,P],--17
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V],--18
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V],--19
            [V,E,V,V,V,V,V,V,V,V,P,V,V,V,V,V,V,E,V,V,V,V],--20
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P]--21
                ]
                
jogo2 :: Jogo
jogo2 = Jogo (Mapa ((0,0),Norte) (0,0) (mapaTradutor mapaDoBoss)) inmjogo2 colecjogo2 jog2