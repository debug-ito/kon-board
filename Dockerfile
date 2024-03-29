FROM debian:bullseye-slim AS build

RUN mkdir /work
RUN mkdir -p /bin
WORKDIR /work

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y ghc cabal-install zlib1g-dev make

COPY kon-board/ ./kon-board/
COPY kon-board-server ./kon-board-server/
COPY kon-board-gen-elm ./kon-board-gen-elm/
COPY cabal.project ./
COPY cabal.project.freeze ./

RUN cabal v2-update
RUN cabal v2-configure -f static --disable-tests --disable-benchmarks
RUN cabal v2-build kon-board-server
RUN cabal v2-install --overwrite-policy=always --install-method=copy --installdir="/bin" kon-board-server

COPY static/ ./static/
COPY Makefile ./
RUN make static/index.html


FROM debian:bullseye-slim

RUN mkdir /server
WORKDIR /server

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y ghc zlib1g
COPY --from=build /bin/kon-board-server /server/
COPY --from=build /work/static/ /server/static/
RUN test -e /server/static/index.html
RUN test -e /server/static/main*.js

ENTRYPOINT ["/server/kon-board-server"]
