#!groovy

node {

    stage('Checkout') {
        checkout scm
    }

    stage('Compile') {
        sh label: '', script: './build.sh'
    }

    stage('Docker build') {
        GIT_COMMIT = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
//		sh "docker build . -t docker.sweng.au.dk/hsbefmi:$GIT_COMMIT"

        withCredentials([[$class          : 'UsernamePasswordMultiBinding', credentialsId: 'nexusjenkinsdocker',
                          usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD']]) {

            sh 'docker login -u $USERNAME -p $PASSWORD https://docker.sweng.au.dk'


            docker.withRegistry('https://docker.sweng.au.dk', 'nexusjenkinsdocker') {


                def im = docker.build("hsbefmi:${GIT_COMMIT}")
                //im.push()

				sh "docker push docker.sweng.au.dk/hsbefmi:${GIT_COMMIT}"

            }
        }
    }
}


