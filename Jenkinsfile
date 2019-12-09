#!groovy

pipeline {
    agent any

    options { buildDiscarder(logRotator(numToKeepStr: '5')) }

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Compile') {
            steps {
                script {
                    //  sh label: '', script: './build.sh'
                    docker.image('docker.sweng.au.dk/haskell865nodeelm:latest').inside('-u jenkins:jenkins') {
                        sh './make.sh none'
                    }
                }
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


        stage('Push to production') {
            steps {
                script {
                    if (env.BRANCH_NAME == 'master') {
                        build job: 'fmi_utilities_deploy', parameters: [[$class: 'StringParameterValue', name: 'DOCKER_IMAGE_VERSION', value: GIT_COMMIT]]
                    }
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



