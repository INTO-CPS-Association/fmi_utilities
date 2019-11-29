#!groovy

node{

	stage ('Checkout'){
		checkout scm
	}

	stage ('Compile'){
sh label: '', script: './build.sh'
	}

	stage ('Docker build'){
GIT_COMMIT = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
//		sh "docker build . -t docker.sweng.au.dk/hsbefmi:$GIT_COMMIT"

docker.withRegistry('https://docker.sweng.au.dk', 'nexus_docker_upp') {


def im = docker.build("docker.sweng.au.dk/hsbefmi:${GIT_COMMIT}")
im.push()



}
	}
}


