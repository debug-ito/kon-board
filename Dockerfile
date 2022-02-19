FROM debian:bullseye-slim AS build

RUN mkdir /work
WORKDIR /work

RUN apt-get update
RUN apt-get upgrade
RUN apt-get install -y ghc cabal-install zlib1g-dev

COPY kon-board/ ./kon-board/
COPY kon-board-server ./kon-board-server/
COPY kon-board-gen-elm ./kon-board-gen-elm/
COPY cabal.project ./
COPY cabal.project.freeze ./

RUN cabal update
RUN cabal configure -f static --disable-tests --disable-benchmarks
RUN cabal build


FROM debian:bullseye-slim

RUN mkdir /server
WORKDIR /server

RUN apt-get update
RUN apt-get upgrade
RUN apt-get install -y ghc zlib1g

------ TODO: where is the executable?

COPY --from=build /work/kon-board-server/dist/build/kon-board-server /server/
COPY static/ /server/static/
RUN test -e /server/static/main.js

ENTRYPOINT ["/server/kon-board-server"]
