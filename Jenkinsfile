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

		def image = docker.build("hsbefmi:${GIT_COMMIT}")
        withCredentials([[$class          : 'UsernamePasswordMultiBinding', credentialsId: 'nexusjenkinsdocker',
                          usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD']]) {

			sh 'docker login -u $USERNAME -p $PASSWORD https://docker.sweng.au.dk'


			docker.withRegistry("https://docker.sweng.au.dk", "nexusjenkinsdocker") {


				image.push()
				image.push('latest')

				//sh "docker push docker.sweng.au.dk/hsbefmi:${GIT_COMMIT}"

			}
		}


    }

    stage('Remove local images') {
        // remove docker images
        sh("docker rmi -f docker.sweng.au.dk/hsbefmi::latest || :")
        sh("docker rmi -f docker.sweng.au.dk/hsbefmi:${GIT_COMMIT} || :")
    }
    //}
}


