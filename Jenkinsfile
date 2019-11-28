#!groovy

node{

	stage ('Checkout'){
		checkout scm
	}

	stage ('Compile'){
sh label: '', script: './build.sh'
	}

	stage ('Docker build'){
		sh "bash docker-build.sh"
	}
}


