FROM debian:buster-slim AS build

RUN mkdir /work
WORKDIR /work

RUN apt-get update
RUN apt-get upgrade
RUN apt-get install -y ghc cabal-install zlib1g-dev
RUN cabal update

COPY kon-board/ ./kon-board/
COPY lts-12.26-cabal.config ./kon-board/cabal.config

WORKDIR /work/kon-board
RUN cabal sandbox init
RUN cabal install --only-dependencies --disable-tests --disable-benchmarks --disable-documentation
RUN cabal configure -f static --disable-tests --disable-benchmarks
RUN cabal build kon-board-server

ENTRYPOINT ["bash"]

## FROM debian:buster-slim
## 
## RUN mkdir /work
## WORKDIR /work
## 
## RUN apt-get update
## RUN apt-get upgrade
## RUN apt-get install -y ghc zlib1g
## 
## COPY --from=build /work/kon-board/dist/build/kon-board-server /
