#!groovy

node{

	stage ('Checkout'){
		checkout scm
	}

	stage ('Compile'){
sh label: '', script: './build.sh'
	}

	stage ('Docker build'){
		sh "docker build . -t docker.sweng.au.dk/hsbefmi:$GIT_COMMIT"
	}
}


