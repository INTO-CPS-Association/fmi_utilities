#!/usr/bin/env bash

# Execute script from root location

# Full path to FMU
FMU=$(readlink -f ${1})
RESULTSDIR="${2}"
N=$3
L=$4

# Go to directory with FMU Analyzer jars
cd fmuanalyzer

# Store directory with jars
CURDIR=$(pwd)
#echo "CURDIR: ${CURDIR}"

# Create the resultsdir, if it does not exist.
mkdir -p ${RESULTSDIR}

# ResultsProcessing*.jar requires that experiment.log is located a folder up from the results.
java  -D"org.slf4j.simpleLogger.defaultLogLevel=trace"  -jar FMUAnalyzer-assembly-0.1.jar mbt -d ${FMU} -n ${N} -l ${N} -o ${RESULTSDIR}/results > ${RESULTSDIR}/experiment.log 2>&1

# Go to RESULTSDIR an execute ResultsProcessing*.jar as it requires to be in the same folder as experiment.log
cd ${RESULTSDIR}
java -cp ${CURDIR}/ResultsProcessing-assembly-0.1.jar ProcessMBTResults

zip -r -q results.zip results experiment.log
