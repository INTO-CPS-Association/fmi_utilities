# HsBeFMI

This repository contains the backend and frontend (as a submodule) for the FMI
Model Description checker
https://github.com/INTO-CPS-Association/FMI2-VDM-Model.

The backend is developed in Haskell using the Scotty framework.

The frontend is developed in Elm and uses Twitter Bootstrap for styling.

# Setting up
Download Stack (For the Haskell backend) https://docs.haskellstack.org/en/stable/README/
Download Elm (For the Elm frontend) https://guide.elm-lang.org/install.html

# Building

## Manually
Go the the directory root/ElmFeFMI and run the command: `elm make src/Main.elm
--output=main.js` 
Go to root/ and run the command: `stack run` - hosts the backend on port 3000.

## Make
Execute `make.sh`, which performs the steps ab

# Testing
To send a test file manually to the server, one can use the tool curl.
Example - execute in root: `curl -F "data=@testdata/modelDescription.xml" http://localhost:3000/fmichecker`
