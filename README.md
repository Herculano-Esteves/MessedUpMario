# Laboratórios de Informática I

## Configuração Local (Sem "Bloat")

Este projeto foi configurado para utilizar um armazenamento de bibliotecas local, para evitar instalar pacotes globais no computador.

Para manter o armazenamento dentro desta pasta, certifique-se de que tem um ficheiro `cabal.project` na raiz do projeto com o seguinte conteúdo:

```cabal
packages: .
store-dir: ./.cabal-store
allow-newer: base
```

Isso garante que todas as bibliotecas (incluindo `gloss` e `OpenGLRaw`) fiquem na pasta `.cabal-store` dentro deste projeto.

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do `cabal`.

```bash
cabal run primate-kong
```

## Interpretador

Pode abrir o interpretador do Haskell (GHCi) utilizando o cabal com o projecto automaticamente carregado.

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Pode correr os testes utilizando o seguinte comando.

```bash
cabal test
```

Se pretender executar os exemplos da documentação como testes unitários utiliza-se a biblioteca [Doctest](https://hackage.haskell.org/package/doctest).

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

## Documentação

Pode gerar a documentação com o [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
