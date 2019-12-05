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

    post {

        cleanup {
            // remove docker images
            sh("docker rmi -f docker.sweng.au.dk/hsbefmi::latest || true")
            sh("docker rmi -f docker.sweng.au.dk/hsbefmi:${GIT_COMMIT} || true")
        }
    }
}



