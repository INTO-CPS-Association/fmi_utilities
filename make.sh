#!/usr/bin/env bash

TARGETDIR="${PWD}/target"
TARGETFRONTEND="${TARGETDIR}/frontend"
TARGETINDEXFILE="$TARGETFRONTEND/index.html"
echo "TARGETDIR: ${TARGETDIR}"
echo "TARGETFRONTEND: ${TARGETFRONTEND}"
echo "TARGETINDEXFILE: ${TARGETINDEXFILE}"


rm -rf ${TARGETDIR}

function empty_base_url(){
    echo "Setting empty base_url"
    sed -i '' -e 's#{{ BASE_URL }}##g' ${TARGETINDEXFILE}
}

function copy_frontend(){
    mkdir --parents $1
    cp --verbose ./frontend/index.html ./frontend/main.js $1
}


# MAKE ELM
echo "================================================================================="
echo "=== Building frontend ==="
echo "================================================================================="
(cd ./frontend;\
    elm make src/Main.elm --output=main.js)

echo "=== Copying frontend to target and setting {{ BASE_URL }}"
if [ ! -z $1 ]
then
    if [ $1 == "local" ]
    then
        copy_frontend ${TARGETFRONTEND}
        empty_base_url
    elif [ $1 == "localprod" ]
    then
        copy_frontend ${TARGETFRONTEND}/fmiutils
        echo "Setting BASE_URL to /fmiutils"
        sed -i '' -e 's#{{ BASE_URL }}#http://localhost/fmiutils/#g' ${TARGETINDEXFILE}
    elif [ $1 == "prod" ]
    then
        copy_elm_to_dir ${TARGETFRONTEND}
        echo "Setting BASE_URL to /fmiutils"
        sed -i '' -e 's#{{ BASE_URL }}#https://sweng.au.dk/fmiutils/#g' ${TARGETINDEXFILE}
    else
        echo "Unknown argument: ${1}"
        exit 1
    fi
else
    copy_frontend ${TARGETFRONTEND}
    empty_base_url
fi
echo "================================================================================="
echo "=== Building backend and copying to target ==="
echo "================================================================================="

(cd ./backend;\
    stack build --local-bin-path ${TARGETDIR} --copy-bins;\
    cp --verbose ./appconfig.json ${TARGETDIR})

echo "================================================================================="
echo "=== Copying VDMCheck and FMUAnalyzer ==="
echo "================================================================================="
cp -r --verbose ./fmuanalyzer ./vdmcheck-0.0.1 ${TARGETDIR}
