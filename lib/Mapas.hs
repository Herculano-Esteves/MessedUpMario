module Mapas where

import LI12324
import Graphics.Gloss
import Utilities
import Tarefa1



generateInicialHitbox :: Mapa -> Hitbox
generateInicialHitbox (Mapa ((x,y),b) _ _) = ((x-4,y-3),(x+4,y+3))



initialState :: State
initialState = State {
    levels = [
        (jogoSamp, True),
        (jogo1, False),
        (jogo2,False)--,
        --(jogoTurorial,False)
        ],
    initLevel = jogoSamp,
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
    images = [],
    animTime = 2,
    editorState = EditorState {
        tempGame = jogoSamp,
        levelEditorPos = (0.5,0.5),
        selectFunc = 0,
        removingEnemies = False,
        savingGame = False
    },
    cheats = False,
    screenSize = (0,0)
}

data Letra
    = P
    | E
    | A
    | V
    | T
    | O
    | D

mapaTutorialLetras :: [[Letra]]
mapaTutorialLetras = 
    [
        [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P],
        [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [V,V,V,V,V,V,V,V,D,D,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P],
        [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],
        [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P]
    ]



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
                D -> Espinho


mapaTeste = Mapa ((6.0,5.5), Oeste) (0.5, 5.5)
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

cuspopersonagem = Personagem {velocidade = (0,0), 
                    tipo = CuspoDeFogo, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (-100.5,-100.5), 
                    tamanho = (0.35,0.35), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)}

barrilpersonagem =Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (-5,0), 
                    tamanho = (0.9,0.9), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)}

inm :: [Personagem]
inm = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (13.5,13.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (19.5,5.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (9.5,9.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
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
                    temChave = False,
                    mira= (0,0)}
                    ]
jog :: Personagem
jog = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (5.5,5.5), 
                    tamanho = (0.6,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False,
                    mira= (0,0)}


-- TESTE DATA END

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda,(2.5,13.5)),(Martelo,(7.5,9.5)),(Chave,(4.5,13.5)),(Estrela,(2.5,5.5))]

jogoSamp ::Jogo
jogoSamp = Jogo mapaTeste inm colec jog 3 (generateInicialHitbox mapaTeste) 0 False

emptyMap :: Mapa
emptyMap = Mapa ((2,2),Norte) (0,0) [[]]

mapa1 = Mapa ((3, 3), Oeste) (0.5, 2.5)
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
jogo1 = Jogo mapa1 inm colec jog 3 (generateInicialHitbox mapa1) 0 False



--Inicio Jogo2
jog2 :: Personagem
jog2 = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (3.0,3.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False,
                    mira= (0,0)}
inmjogo2 :: [Personagem]
inmjogo2 = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (7.5,15.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (3.5,19.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (20.5,7.5), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)},
        Personagem {velocidade = (0,0), 
                    tipo = EyeBoss, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (10.5,10.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 6), 
                    direcao = Oeste,
                    temChave = False,
                    mira= (0,0)}
                    ]

colecjogo2 :: [(Colecionavel, Posicao)]
colecjogo2 = [(Moeda,(2.5,17.5)),(Martelo,(7.5,15.5)),(Chave,(4.5,11.5)),(Estrela,(2.5,7.5))]

mapaDoBoss :: [[Letra]]
mapaDoBoss =    [
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P],--1
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--2
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--3
            [V,V,V,V,V,T,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],--4
            [P,P,P,P,V,P,P,P,P,P,P,P,P,P,P,P,P,P,A,A,A,P,P],--5
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V,V],--6
            [V,V,V,V,V,V,V,V,V,V,V,V,O,V,V,V,E,V,V,V,V,V,V],--7
            [V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,E,V,V,V,V,V,V],--8
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,V,V,P,P,P,P,P,P,P],--9
            [V,V,V,V,V,V,V,V,P,V,V,V,P,E,V,V,V,V,V,V,V,V,V],--10
            [V,V,V,V,V,V,V,V,P,V,V,V,P,E,V,V,V,V,V,V,V,V,V],--11
            [V,V,V,V,V,V,V,V,P,V,V,V,P,V,V,V,V,V,V,V,V,V,V],--12
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,V,V,P,P,P],--13
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V,V],--14
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V,V],--15
            [V,E,V,V,P,V,V,V,V,V,V,V,V,V,V,P,V,E,V,V,V,V,V],--16
            [V,E,V,V,P,P,P,P,P,P,P,P,P,P,P,P,V,E,V,V,P,P,P],--17
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V,V],--18
            [V,E,V,V,V,V,V,V,E,V,P,V,E,V,V,V,V,E,V,V,V,V,V],--19
            [V,E,V,V,V,V,V,V,V,V,P,V,V,V,V,V,V,E,V,V,V,V,V],--20
            [P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P]--21
                ]
                
jogo2 :: Jogo
jogo2 = Jogo mapa inmjogo2 colecjogo2 jog 3 (generateInicialHitbox mapa) 0 False
        where mapa = Mapa ((3.0,3.5),Norte) (0,0) (mapaTradutor mapaDoBoss)

jogoTurorial :: Jogo
jogoTurorial = Jogo mapa [cuspopersonagem] colecjogo2 jog 3 (generateInicialHitbox mapa) 0 False
            where mapa = Mapa ((3,3.5),Norte) (0,0) (mapaTradutor mapaTutorialLetras)