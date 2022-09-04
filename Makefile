ELM_SRC="kon-elm/src"
VERSION := $(shell perl -nle 'if(/^\s*version:\s*(\S+)/) { print $$1 }' < kon-board/kon-board.cabal )

all: backend frontend

backend:
	cabal v2-build kon-board-server

frontend: static/main.js

$(ELM_SRC)/Bridge.elm:
	echo $(VERSION)
	cabal v2-run kon-board-gen-elm $(ELM_SRC)

static/main.js: $(ELM_SRC)/Bridge.elm
	cd kon-elm && elm make src/Main.elm --output=../$@

test: $(ELM_SRC)/Bridge.elm
	cabal v2-test --job=1 all && ( cd kon-elm && npx elm-test )

run: static/main.js
	cabal v2-run kon-board-server

clean:
	rm -rf $(ELM_SRC)/Bridge.elm static/main.js; cabal v2-clean
