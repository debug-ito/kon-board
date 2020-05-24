FROM debian:buster-slim AS build

RUN mkdir /work
WORKDIR /work

RUN apt-get update
RUN apt-get upgrade
RUN apt-get install -y ghc cabal-install zlib1g-dev
RUN cabal update

COPY kon-board/ ./kon-board/
COPY kon-board-server ./kon-board-server/
COPY lts-12.26-cabal.config ./kon-board-server/cabal.config

WORKDIR /work/kon-board-server
RUN cabal sandbox init
RUN cabal sandbox add-source ../kon-board
RUN cabal install --only-dependencies --disable-tests --disable-benchmarks --disable-documentation
RUN cabal configure -f static --disable-tests --disable-benchmarks
RUN cabal build



FROM debian:buster-slim

RUN mkdir /server
WORKDIR /server

RUN apt-get update
RUN apt-get upgrade
RUN apt-get install -y ghc zlib1g

COPY --from=build /work/kon-board-server/dist/build/kon-board-server /server/
COPY static/ /server/static/
RUN test -e /server/static/main.js

ENTRYPOINT ["/server/kon-board-server"]
