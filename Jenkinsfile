#!groovy

node{

	stage ('Checkout'){
		checkout scm
	}

	stage ('Compile'){
		sh "bash build.sh"
	}

	stage ('Docker build'){
		sh "bash docker-build.sh"
	}
}


