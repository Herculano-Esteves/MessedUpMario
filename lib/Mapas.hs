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
        (jogo01, True),
        (jogo2,False),
        (jogo04,False)--,
        --(jogoTurorial,False)
        ],
    initLevel = jogo01,
    currentLevel = 0,
    currentMenu = MainMenu,
    time = 0,
    options = Options {
        currentTheme = Default,
        marioTheme = Default,
        platformTheme = Default
        },
    menuState = MenuState {
        selectedButton = 0,
        pressingButton = False
    },
    exitGame = False,
    images = [],
    animTime = 2,
    editorState = EditorState {
        tempGame = jogo01,
        levelEditorPos = (0.5,0.5),
        selectFunc = 0,
        removingEnemies = False,
        savingGame = False
    },
    cheats = False,
    screenSize = (0,0)
}
--PERSONAGENS START

fantasma :: Personagem
fantasma = Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

macacomalvado :: Personagem
macacomalvado = Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.9,0.9), 
                    aplicaDano = (False, 6), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

eyeentidade :: Personagem
eyeentidade = Personagem {velocidade = (0,0), 
                    tipo = EyeEntidade, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.5,0.7), 
                    aplicaDano = (False, 4), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

eyeboss :: Personagem
eyeboss = Personagem {velocidade = (0,0), 
                    tipo = EyeBoss, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (3,3), 
                    aplicaDano = (False, 4), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

cuspopersonagem :: Personagem
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
                    mira = (False,0,0)}

barrilpersonagem :: Personagem
barrilpersonagem = Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (-5,0), 
                    tamanho = (0.7,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}


jog :: Personagem
jog = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (0.5,0.5), 
                    tamanho = (0.6,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False,
                    mira = (False,0,0)}


-- TESTE DATA END

jogo01 ::Jogo
jogo01 = Jogo {mapa = Mapa ((4.5,5.5),Oeste) (0.5,13.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Espinho,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Porta,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (13.5,13.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (9.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (19.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (1.5,1.5), direcao = Oeste, tamanho = (0.9,0.9), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0,0)}], colecionaveis = [(Moeda,(2.5,13.5)),(Martelo,(7.5,9.5)),(Chave,(4.5,13.5)),(Estrela,(0.5,13.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}
emptyMap :: Mapa
emptyMap = Mapa ((2,2),Norte) (0,0) [[]]
          
jogo2 :: Jogo
jogo2 = Jogo {mapa = Mapa ((2.5,2.5),Oeste) (2.5,7.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Tunel,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Escada,Vazio,Vazio,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (3.5,19.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (10.5,15.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (20.5,7.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = EyeBoss, posicao = (10.5,10.5), direcao = Oeste, tamanho = (3.0,3.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0,0)}], colecionaveis = [(Martelo,(21.5,3.5)),(Moeda,(21.5,7.5)),(Moeda,(21.5,15.5)),(Moeda,(14.5,15.5)),(Moeda,(7.5,11.5)),(Moeda,(0.5,19.5)),(Martelo,(7.5,15.5)),(Chave,(4.5,11.5)),(Estrela,(2.5,7.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.5,7.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}

jogo04 :: Jogo
jogo04 = Jogo {mapa = Mapa ((2.5,4.5),Oeste) (13.5,12.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Tunel,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (12.5,1.5), direcao = Oeste, tamanho = (0.9,0.9), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (14.5,15.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (16.5,21.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (27.5,17.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (29.5,13.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (27.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (27.5,13.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (16.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (25.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (22.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (6.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (13.5,13.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (19.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)}], colecionaveis = [(Moeda,(28.5,17.5)),(Martelo,(28.5,13.5)),(Moeda,(28.5,9.5)),(Martelo,(12.5,9.5)),(Chave,(28.5,5.5)),(Chave,(1.5,9.5)),(Estrela,(13.5,12.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (4.5,1.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0,0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}