module Main where

import Test.HUnit
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import LI12324
import Mapas (mapa1, jogoSamp)
test_suite_01 = test [  "Valida jogoSamp" ~: True ~=? valida jogoSamp,
    "Movimenta" ~: Jogo {mapa = Mapa ((6.0,5.5),Oeste) (0.5,5.5) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Vazio,Escada,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Porta,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,1.0), tipo = Barril, posicao = (1.4666666666666666,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False},Personagem {velocidade = (-2.0,0.0), tipo = MacacoMalvado, posicao = (1.4666666666666666,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (True,8.0), temChave = False},Personagem {velocidade = (-1.5,0.16666666666666666), tipo = Fantasma, posicao = (13.5,13.5), direcao = Oeste, tamanho = (0.75,0.75), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False},Personagem {velocidade = (-1.5,0.16666666666666666), tipo = Fantasma, posicao = (19.5,5.5), direcao = Oeste, tamanho = (0.75,0.75), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False},Personagem {velocidade = (1.5,0.16666666666666666), tipo = Fantasma, posicao = (9.5,9.5), direcao = Oeste, tamanho = (0.75,0.75), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0), temChave = False}], colecionaveis = [(Moeda,(2.5,13.5)),(Martelo,(7.5,9.5)),(Chave,(4.5,13.5)),(Estrela,(2.5,5.5))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (0.8,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0), temChave = False}, lostGame = False} ~=? movimenta 3 (1/60) jogoSamp
                    ]











main :: IO ()
main = runTestTTAndExit $ test [test_suite_01]
