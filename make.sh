#!/usr/bin/env bash

TARGETDIR="${PWD}/target"
TARGETFRONTEND="${TARGETDIR}/frontend"
TARGETINDEXFILE="$TARGETFRONTEND/index.html"
echo "TARGETDIR: ${TARGETDIR}"
echo "TARGETFRONTEND: ${TARGETFRONTEND}"
echo "TARGETINDEXFILE: ${TARGETINDEXFILE}"

#if [ -d ${TARGETDIR} ]
  rm -rf ${TARGETDIR}
#fi
rm -rf frontend/elm-stuff
rm frontend/main.js

function copy_frontend(){
    echo "Copying frontend to: ${1}"

    mkdir --parents $1
    cp --verbose ./frontend/index.html ./frontend/main.js $1
}

function local_copy() {
    copy_frontend ${TARGETFRONTEND}
    echo "Setting empty base_url on: ${TARGETINDEXFILE}"
    sed -i '' -e 's#{{ BASE_URL }}#/#g' ${TARGETINDEXFILE}
}


# MAKE ELM
echo "================================================================================="
echo "=== Building frontend ==="
echo "================================================================================="
(cd ./frontend;\
    elm make src/Main.elm --output=main.js)

echo "=== Copying frontend to target and setting {{ BASE_URL }} ==="
if [ ! -z $1 ]
then
    if [ $1 == "local" ]
    then
        local_copy
    elif [ $1 == "localprod" ]
    then
        LOCALPRODFRONTEND="${TARGETFRONTEND}/fmiutils"
        LOCALPRODFRONTENDINDEX="${LOCALPRODFRONTEND}/index.html"
        copy_frontend ${LOCALPRODFRONTEND}
        echo "Attempting to sed on: ${LOCALPRODFRONTENDINDEX}"
        LOCALPRODSEDEXPR="s#{{ BASE_URL }}#http://localhost/fmiutils/#g"
        if [[ "$OSTYPE" == "darwin"* ]]
        then
            # Mac OSX
            sed -i '' -e "$LOCALPRODSEDEXPR" ${LOCALPRODFRONTENDINDEX}
        else
            sed -i -e "$LOCALPRODSEDEXPR" ${LOCALPRODFRONTENDINDEX}
        fi
    elif [ $1 == "prod" ]
    then
        copy_frontend ${TARGETFRONTEND}
        echo "Setting BASE_URL to /fmiutils/"
        sed -i -e 's#{{ BASE_URL }}#https://sweng.au.dk/fmiutils/#g' ${TARGETINDEXFILE}
    elif [ $1 == "none" ]
    then
        copy_frontend ${TARGETFRONTEND}
    else
        echo "Unknown argument: ${1}"
        exit 1
    fi
else
    local_copy
fi
echo "================================================================================="
echo "=== Building backend and copying to target ==="
echo "================================================================================="

(cd ./backend;\
    stack build --local-bin-path ${TARGETDIR} --copy-bins --allow-different-user ;\
    cp --verbose ./appconfig.json ${TARGETDIR})

echo "================================================================================="
echo "=== Copying VDMCheck and FMUAnalyzer ==="
echo "================================================================================="
echo "COPYING TO: ${TARGETDIR}"
cp -r --verbose ./fmuanalyzer ./vdmcheck-0.0.2 ${TARGETDIR}
cp -r --verbose ./fmuanalyzer ./vdmcheck-0.0.3 ${TARGETDIR}
