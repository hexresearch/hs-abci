# Haskell Tendermint Interface (ABCI)

Build blockchain applications in Haskell for
[Tendermint](https://tendermint.com/)

[![Build Status](https://travis-ci.org/albertov/hs-abci.svg?branch=master)](https://travis-ci.org/albertov/hs-abci)

## Pre-Install

1. Clone this repository

  ```sh
  git clone https://github.com/albertov/hs-abci
  cd hs-abci
  ```

## Install

### With stack

1. Install [stack](https://www.haskellstack.org)
1. Run `stack install`

### With cabal

1. Make sure cabal-install is somewhere in the `PATH`
1. Run `cabal install`

## Run test suite

1. Install [stack](https://www.haskellstack.org)
1. Run `stack test`


## Update

1. Checkout http://github.com/tendermint/abci commit used by tendermint
1. Copy `abci/types/types.proto` from it to `src/Network/ABCI/types.proto`
1. `nix-shell -p haskellPackages.proto-lens-protoc -p protobuf3_4`
1. `export GOPATH=$(pwd)/../gohome`
1. `mkdir -p $GOPATH`
1. `cd $GOPATH`
1. `go get github.com/tendermint/tmlibs/common`
1. `go get github.com/gogo/protobuf/gogoproto`
1.  ```
    protoc --plugin=protoc-gen-haskell=$(which proto-lens-protoc) \
        --haskell_out=../hs-abci/src --proto_path ./src \
        Network/ABCI/types.proto \
        github.com/gogo/protobuf/gogoproto/gogo.proto \
        github.com/tendermint/tmlibs/common/types.proto
    ```


1. commit files
    - src/Network/ABCI/types.proto
    - src/Proto/Network/ABCI/Types.hs
