# HsBeFMI
This repository contains the backend and frontend for INTO-CPS FMI Utilities

The FMIChecker source is available at 
https://github.com/INTO-CPS-Association/FMI2-VDM-Model.

The FMUAnalyzer source is available at https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER. 

The backend is developed in Haskell using the Scotty framework.

The frontend is developed in Elm and uses Twitter Bootstrap for styling.

# Setting up
Download Stack (For the Haskell backend) https://docs.haskellstack.org/en/stable/README/
Download Elm (For the Elm frontend) https://guide.elm-lang.org/install.html

# Building
1. Execute make.sh
Note: The warning `sed: can't read : No such file or directory` occurring on
linux can be ignored safely.
2. Go to target
3. Execute HsBeFMI-exe

# Testing
To send a test file manually to the server, one can use the tool curl.
Example - execute in root: `curl -F "data=@testdata/modelDescription.xml" http://localhost/api/fmichecker`

# Starting on server:
`sudo nohup ./HsBeFMI-exe`


# Jenkins and Docker

The jenkins file will build using docker and generate an image with the git commit version.

Is done locally then a container can be started with

```bash
docker run -p 8888:80 hsbefmi:<GIT COMMIT>
```

and if needed a shell can be attached using 

```bash
docker exec -i -t <container id> /bin/sh
```
