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
        (jogo02,True),
        (jogo03,True),
        (jogo04,True)--,
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

bowser :: Personagem
bowser = Personagem {velocidade = (0,0), 
                    tipo = Boss, 
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
                    posicao = (0,0), 
                    tamanho = (0.35,0.35), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

bolacanhao :: Personagem
bolacanhao = Personagem {velocidade = (0,0), 
                    tipo = BolaDeCanhao, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.4,0.4), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

canhao :: Personagem
canhao = Personagem {velocidade = (0,0), 
                    tipo = Canhao, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.7,0.7), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

atiradorbase :: Personagem
atiradorbase = Personagem {velocidade = (0,0), 
                    tipo = AtiradorBase, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (1,2), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

atiradorfogueteent :: Personagem
atiradorfogueteent = Personagem {velocidade = (0,0), 
                    tipo = AtiradorFoguete, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.5,0.5), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False,
                    mira = (False,0,0)}

caoinimigo :: Personagem
caoinimigo = Personagem {velocidade = (0,0), 
                    tipo = CaoEnemy, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (0,0), 
                    tamanho = (0.7,0.7), 
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

jogo05 ::Jogo
jogo05 = Jogo {mapa = Mapa ((4.5,5.5),Oeste) (0.5,13.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Espinho,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Porta,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (11.5,8.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (11.5,4.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (19.5,13.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (16.5,13.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (13.5,13.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (9.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (19.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (1.5,1.5), direcao = Oeste, tamanho = (0.9,0.9), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0.0,0.0)}], colecionaveis = [(Moeda,(2.5,13.5)),(Martelo,(7.5,9.5)),(Chave,(4.5,13.5)),(Estrela,(0.5,13.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}, lostGame = 3, cameraControl = ((6.666666666666666,4.0),(14.666666666666666,10.0)), animacaoJogo = 0.0, cheatsjogo = False}

emptyMap :: Mapa
emptyMap = Mapa ((2,2),Norte) (0,0) [[]]

jogo01 :: Jogo
jogo01 = Jogo {mapa = Mapa ((2.5,4.5),Oeste) (1.5,13.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Tunel,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (14.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (20.5,6.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (15.5,14.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (8.5,14.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (4.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (9.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (1.5,14.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (17.5,9.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (23.5,5.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (0.5,13.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (6.5,1.5), direcao = Oeste, tamanho = (0.9,0.9), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0.0,0.0)}], colecionaveis = [(Moeda,(8.5,14.5)),(Moeda,(17.5,14.5)),(Moeda,(11.5,14.5)),(Moeda,(22.5,5.5)),(Moeda,(21.5,5.5)),(Moeda,(4.5,14.5)),(Moeda,(7.5,9.5)),(Chave,(2.5,9.5)),(Estrela,(1.5,13.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (5.5,4.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}

jogo02 :: Jogo
jogo02 = Jogo {mapa = Mapa ((2.5,2.5),Oeste) (2.5,7.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Tunel,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Escada,Vazio,Vazio,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (10.5,15.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (0.5,18.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (22.5,18.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (22.5,11.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (3.5,19.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (20.5,7.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = EyeBoss, posicao = (10.5,10.5), direcao = Oeste, tamanho = (3.0,3.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0.0,0.0)}], colecionaveis = [(Moeda,(6.5,18.5)),(Moeda,(5.5,18.5)),(Moeda,(5.5,10.5)),(Moeda,(7.5,10.5)),(Moeda,(6.5,10.5)),(Moeda,(6.5,6.5)),(Moeda,(7.5,6.5)),(Moeda,(22.5,7.5)),(Moeda,(22.5,6.5)),(Moeda,(21.5,6.5)),(Moeda,(14.5,13.5)),(Moeda,(13.5,13.5)),(Moeda,(13.5,14.5)),(Moeda,(14.5,14.5)),(Moeda,(13.5,15.5)),(Moeda,(5.5,19.5)),(Moeda,(6.5,19.5)),(Moeda,(5.5,11.5)),(Moeda,(6.5,11.5)),(Martelo,(10.5,15.5)),(CogumeloVida,(5.5,15.5)),(Moeda,(6.5,7.5)),(Moeda,(7.5,7.5)),(Martelo,(21.5,3.5)),(Moeda,(21.5,7.5)),(Moeda,(21.5,15.5)),(Moeda,(14.5,15.5)),(Moeda,(7.5,11.5)),(Martelo,(7.5,15.5)),(Chave,(4.5,11.5)),(Estrela,(2.5,7.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (5.5,3.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}

jogo03 :: Jogo
jogo03 = Jogo {mapa = Mapa ((4.5,2.5),Oeste) (37.5,6.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Tunel,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (39.5,5.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (37.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (34.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (31.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (28.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (25.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (22.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (19.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (16.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (13.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (10.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (7.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (4.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (1.5,10.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}], colecionaveis = [(Moeda,(30.5,4.5)),(Moeda,(30.5,5.5)),(Moeda,(31.5,5.5)),(Moeda,(29.5,5.5)),(Moeda,(30.5,6.5)),(CogumeloVida,(22.5,6.5)),(Moeda,(16.5,6.5)),(Martelo,(15.5,6.5)),(Moeda,(15.5,5.5)),(Moeda,(16.5,5.5)),(Moeda,(16.5,4.5)),(Moeda,(15.5,4.5)),(Moeda,(9.5,5.5)),(Moeda,(8.5,5.5)),(Moeda,(8.5,6.5)),(Moeda,(9.5,6.5)),(Estrela,(37.5,6.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (2.5,5.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}, lostGame = 3, cameraControl = ((6.666666666666666,3.0),(14.666666666666666,9.0)), animacaoJogo = 0.0, cheatsjogo = False}


jogo04 :: Jogo
jogo04 = Jogo {mapa = Mapa ((20.5,2.5),Oeste) (20.5,23.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Tunel,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Espinho,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (16.5,24.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (13.5,23.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = EyeEntidade, posicao = (19.5,19.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,4.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (13.5,19.5), direcao = Oeste, tamanho = (0.5,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (20.5,15.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Canhao, posicao = (19.5,15.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (9.5,8.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (9.5,22.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (19.5,20.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (9.5,16.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (15.5,12.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = AtiradorBase, posicao = (21.5,6.5), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = CaoEnemy, posicao = (9.5,4.5), direcao = Oeste, tamanho = (0.7,0.7), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Boss, posicao = (1.5,4.5), direcao = Oeste, tamanho = (0.9,0.9), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,6.0), temChave = False, mira = (False,0.0,0.0)}], colecionaveis = [(Moeda,(20.5,6.5)),(Moeda,(20.5,7.5)),(Moeda,(15.5,23.5)),(Moeda,(15.5,22.5)),(Moeda,(14.5,22.5)),(Moeda,(14.5,23.5)),(Moeda,(20.5,17.5)),(Moeda,(21.5,17.5)),(Moeda,(21.5,18.5)),(Moeda,(20.5,18.5)),(Moeda,(20.5,19.5)),(Martelo,(21.5,19.5)),(CogumeloVida,(16.5,15.5)),(Moeda,(21.5,10.5)),(Moeda,(20.5,10.5)),(Moeda,(20.5,11.5)),(Moeda,(13.5,5.5)),(Moeda,(12.5,5.5)),(Moeda,(12.5,6.5)),(Moeda,(13.5,6.5)),(Martelo,(13.5,7.5)),(Martelo,(10.5,15.5)),(Moeda,(10.5,13.5)),(Moeda,(11.5,13.5)),(Moeda,(11.5,15.5)),(Moeda,(11.5,14.5)),(Moeda,(10.5,14.5)),(Moeda,(11.5,18.5)),(Moeda,(12.5,18.5)),(Moeda,(13.5,18.5)),(Moeda,(13.5,19.5)),(Moeda,(12.5,19.5)),(Moeda,(11.5,19.5)),(Chave,(21.5,11.5)),(Moeda,(12.5,7.5)),(Estrela,(20.5,23.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (16.5,1.5), direcao = Este, tamanho = (0.6,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False, mira = (False,0.0,0.0)}, lostGame = 3, cameraControl = ((7.333333333333334,3.0),(15.333333333333334,9.0)), animacaoJogo = 0.0, cheatsjogo = False}