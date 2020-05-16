# kon-board - meal calendar on your fridge

**Work in progress**

kon-board is a personal Web application of digital signage for your refrigerator.

## Run

**TODO: run the Docker image.**

## Add recipes

**TODO**

## Add meal plans

**TODO** 

## Development

### Content

Content of this repository is:

- kon-board: Haskell program of the backend Web server
- kon-elm: Elm program of the Web frontend application
- static: Static files served by the backend server.

### Build

This project uses the [Haskell stack](https://docs.haskellstack.org/) for backend and [Elm 0.19](https://elm-lang.org/) for frontend. Install those tools to build this project.

#### Backend server

To build the backend server, run

    $ make backend

#### Frontend application

To build the frontend application, run

    $ make frontend

This will generate `static/main.js`, the JavaScript application built from Elm source files in kon-elm.

The Elm program requires some Elm code generated by the Haskell program under kon-board, so `make frontend` also builds the Haskell program.

To build the backend and frontend, just run `make`.

#### Backend Docker image

To build the Docker image, run

    $ make frontend
    $ docker build .

Note:

- You need to build the frontend outside the Docker container. The Dockerfile will copy the pre-built main.js into the container. This is because there is no release of Elm compiler for ARM.
- Dockerfile doesn't use Haskell stack. It uses the system GHC and cabal-install instead. This is because Haskell stack (sometimes) doesn't support ARM.

### Run

In development, run the following after you build the backend and frontend.

    $ stack run kon-board-server

## Author

Toshio Ito <debug.ito@gmail.com>
