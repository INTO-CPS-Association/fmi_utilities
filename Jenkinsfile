#!groovy

pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Compile') {
            steps {
                sh label: '', script: './build.sh'
            }
        }

        stage('Docker build') {
            steps {
                script {
                    GIT_COMMIT = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()

                    docker.withRegistry("https://docker.sweng.au.dk", "nexusjenkinsdocker") {
                        println "Building image"
                        def image = docker.build("docker.sweng.au.dk/hsbefmi:${GIT_COMMIT}")

                        println "pushing image"
                        image.push()
                        println "pushing as latest"
                        image.push('latest')

                    }
                }
            }
        }

        stage('Production Merge Request') {
            steps {
                checkout([$class                           : 'GitSCM',
                          branches                         : [[name: '*/master']],
                          doGenerateSubmoduleConfigurations: false,
                          extensions                       : [[$class           : 'RelativeTargetDirectory',
                                                               relativeTargetDir: 'deploy-host']],
                          submoduleCfg                     : [],
                          userRemoteConfigs                : [[credentialsId: '37a9ff32-3ec0-4408-b5bd-35eda9d383d9', url: 'https://gitlab.au.dk/software-engineering/deploy-hosts/fmi_utilities.git']]])
                dir("deploy-host") {
                    sh "git co -b ${GIT_COMMIT}"
                    sh "echo should sed"
                    sh "git push --set-upstream origin ${GIT_COMMIT}"
                }

                withCredentials([string(credentialsId: 'gitlab_au_builder_token', variable: 'PRIVATE_TOKEN')]) {
                    sh '''
HOST=https://gitlab.au.dk/api/v4/projects/
CI_PROJECT_ID=3762
#software-engineering/deploy-hosts/fmi_utilities
CI_COMMIT_REF_NAME=$GIT_COMMIT

TARGET_BRANCH=`curl --silent "${HOST}${CI_PROJECT_ID}" --header "PRIVATE-TOKEN:${PRIVATE_TOKEN}" | python3 -c "import sys, json; print(json.load(sys.stdin)['default_branch'])"`;


# The description of our new MR, we want to remove the branch after the MR has
# been closed
BODY="{
    \\"id\\": ${CI_PROJECT_ID},
    \\"source_branch\\": \\"${CI_COMMIT_REF_NAME}\\",
    \\"target_branch\\": \\"${TARGET_BRANCH}\\",
    \\"remove_source_branch\\": true,
    \\"title\\": \\"WIP: ${CI_COMMIT_REF_NAME}\\"
}";

# Require a list of all the merge request and take a look if there is already
# one with the same source branch
LISTMR=`curl --silent "${HOST}${CI_PROJECT_ID}/merge_requests?state=opened" --header "PRIVATE-TOKEN:${PRIVATE_TOKEN}"`;
COUNTBRANCHES=`echo ${LISTMR} | grep -o "\\"source_branch\\":\\"${CI_COMMIT_REF_NAME}\\"" | wc -l`;

# No MR found, let's create a new one
if [ ${COUNTBRANCHES} -eq "0" ]; then
    curl -X POST "${HOST}${CI_PROJECT_ID}/merge_requests" \\
        --header "PRIVATE-TOKEN:${PRIVATE_TOKEN}" \\
        --header "Content-Type: application/json" \\
        --data "${BODY}";

    echo "Opened a new merge request: WIP: ${CI_COMMIT_REF_NAME} and assigned to you";
    exit;
fi

echo "No new merge request opened";

                    '''
                }
            }
        }


    }

    post {

        cleanup {
            // remove docker images
            sh("docker rmi -f docker.sweng.au.dk/hsbefmi:latest || true")
            sh("docker rmi -f docker.sweng.au.dk/hsbefmi:${GIT_COMMIT} || true")
        }
    }
}



