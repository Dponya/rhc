# Remote Haskell Call

**RHC** is a Haskell implementation of [JSON-RPC 2.0 Protocol](https://www.jsonrpc.org/specification).

## Goal

At the moment, the library is in the active development stage. The package was designed and developed to follow the specific goals of the author and help him to learn about Haskell's features. So, this package isn't ready for production use yet, and maybe will be ready

## Features

* Convenient server interface for defining remote procedures in namespaces
* Automatic generation of remote procedures from the server on the client side
* Minimal boilerplate that reached by using [Template Haskell](https://hackage.haskell.org/package/template-haskell)

## Usage guide

`examples` folder contains client and server usage examples.

For better convenience, here are those examples:

Client-Side code:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Rhc


load (CliConf 3000 "localhost" Http)
     ["example", "coffee"]

config :: CliConf
config = CliConf
    { cPort = 3000
    , cHost = "localhost"
    , cProtocol = Http
    }

main :: IO ()
main = remoteRunner (makeCoffee "hello from client!") >>= print

```

Server-Side code:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Aeson
import Data.Aeson.Types
import Network.Wai.Handler.Warp (Port)
import Language.Haskell.TH(runQ)

import Rhc

doSome :: RemoteAction [Int] Int
doSome (x:x2:xs) = liftIO $ print x >> pure (x + x2)
doSome xs = liftIO $ print xs >> pure 0

doErr :: RemoteAction [Int] [Int]
doErr (x:x2:xs) = if x > x2
  then throwM (ErrorObject
                (ErrorServCause 
                  (ServerError (-32000)))
                "test message")
  else pure [x + x2]

makeCoffee :: RemoteAction String [String]
makeCoffee x = pure ["some", "pre-defined", "words"]

countCoffee :: RemoteAction String Int
countCoffee _ = pure 55

generate $ domain "example"
                ( method "doSome" 'doSome
               <> method "doErr" 'doErr )
             <>
           domain "coffee"
                ( method "makeCoffee" 'makeCoffee
               <> method "countCoffee" 'countCoffee )

main :: IO ()
main = serv 3000

```

## Development

To build the project and run the tests locally, you can use either
`cabal` or `stack`.

### Cabal

Build the project:

```shell
cabal build rhc
```

To build examples, run first server command:

```shell
cabal build simple-server
```

And then run client command:

```shell
cabal build simple-client
```

The cause for this is that client code queries methods from the server in compile-time to assure in type correctness of methods.

Run all tests:

```shell
cabal test --enable-tests --test-show-details=direct
```

### Stack

Recommended to build `simple-server` first with:

```shell
stack ghci
```

And run the main function to start the server. It will provide the working server for client example compilation and the whole project:

```shell
stack build --test --no-run-tests
```

Run all tests:

```shell
stack test
```