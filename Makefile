ELM_SRC="kon-elm/src"

all: backend frontend

backend:
	cabal v2-build kon-board-server

frontend: static/main.js

$(ELM_SRC)/Bridge.elm:
	cabal v2-run kon-board-gen-elm $(ELM_SRC)

static/main.js: $(ELM_SRC)/Bridge.elm
	cd kon-elm && elm make src/Main.elm --output=../$@

test: $(ELM_SRC)/Bridge.elm
	cabal v2-test all && ( cd kon-elm && npx elm-test )

clean:
	rm -rf $(ELM_SRC)/Bridge.elm static/main.js; cabal v2-clean
