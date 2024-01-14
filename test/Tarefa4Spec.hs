module Tarefa4Spec (testesTarefa4) where

import LI12324
import Tarefa3
import Tarefa4 (atualiza)
import Test.HUnit

mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0),
      temChave = False,
      mira = (False,0,0)
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7.5, 7.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0),
      temChave = False,
      mira = (False,0,0)
    }

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [(Estrela, (8.5,6.5))],
      jogador = jogadorParado,
      lostGame = 3,
      cameraControl = ((0,0),(0,0)),
      animacaoJogo = 0,
      cheatsjogo = False
    }

teste01 :: Test
teste01 = "T01: Quando não há nenhuma acção, o jogo permanece inalterado" ~: jogo01 ~=? atualiza [Nothing] Nothing jogo01

andarDireita01 :: Jogo
andarDireita01 = atualiza [Nothing] (Just AndarDireita) jogo01

teste02 :: Test
teste02 = TestLabel "T02" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarDireita, o vetor velocidade do jogador é positivo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) > 0
    testeB = "B: Quando a acção é AndarDireita, a orientação do jogador é Este" ~: Este ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = movimenta 100 (1/60) $ atualiza [Nothing] (Just AndarDireita) jogo01

teste03 :: Test
teste03 = TestLabel "T03" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarEsquerda, o vetor velocidade do jogador é negativo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) < 0
    testeB = "B: Quando a acção é AndarEsquerda, a orientação do jogador é Oeste" ~: Oeste ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = movimenta 100 (1/60) $ atualiza [Nothing] (Just AndarEsquerda) jogo01{jogador= (jogador jogo01){posicao = (5.5,3.5)}}

teste04 :: Test
teste04 = TestLabel "T04" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Saltar, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSaltar) < 0
    testeB = "B: Quando a acção é Saltar, a orientação do jogador não muda" ~: (direcao . jogador $ jogo01) ~=? (direcao . jogador $ resultadoSaltar)
    resultadoSaltar = movimenta 100 (1/60) $ atualiza [Nothing] (Just Saltar) jogo01{jogador= (jogador jogo01){posicao = (5.5,4.5),tamanho = (1,1)}}

jogadorEmFrenteEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7.5, 7.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0),
      temChave = False,
      mira = (False,0,0)
    }

jogo02 :: Jogo
jogo02 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorEmFrenteEscada,
      lostGame = 3,
      cameraControl = ((0,0),(0,0)),
      animacaoJogo = 0,
      cheatsjogo = False
    }

teste05 :: Test
teste05 = TestLabel "T05" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Subir, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadosubirescada) < 0
    testeB = "B: Quando a acção é Saltar, o jogador passa a estar em escada" ~: True ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = movimenta 100 (1/2) $ atualiza [Nothing,Nothing] (Just Subir) $ movimenta 100 (1/2) jogo01{jogador= (jogador jogo01){posicao = (6.5,4.5),tamanho = (1,1)},inimigos = [inimigoParado{posicao = (1,1)},inimigoParado{posicao = (1,1)}]}
    resultadosubirescada = movimenta 100 (1/60) $ atualiza [Nothing, Nothing] (Just Subir) $ movimenta 100 (1/60) jogo01{jogador= (jogador jogo01){posicao = (6.5,4.5),tamanho = (1,1)},inimigos = [inimigoParado{posicao = (1,1)},inimigoParado{posicao = (1,1)}]}

jogadorEmEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7, 7),
      direcao = Norte,
      tamanho = (0.8, 0.8),
      emEscada = True,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0),
      temChave = False,
      mira = (False,0,0)
    }

teste06 :: Test
teste06 = TestLabel "T06" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Descer, o vetor velocidade do jogador é positivo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSubir) > 0
    testeB = "B: Quando a acção é Descer, o jogador continua em escada" ~: (emEscada jogadorEmEscada) ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = atualiza [Nothing] (Just Descer) (jogo01 {
      jogador = jogadorEmEscada { emEscada = True, posicao = (7,6)}
    })

testesTarefa4 :: Test
testesTarefa4 = TestLabel "Tarefa4 (atualiza)" $ test [teste01, teste02, teste03, teste04, teste05, teste06]
