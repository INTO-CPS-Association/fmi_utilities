#!groovy

node{

	stage ('Checkout'){
		checkout scm
	}

	stage ('Compile'){
		sh "./build.sh"
	}

	stage ('Docker build'){
		sh "./docker-build.sh"
	}
}


