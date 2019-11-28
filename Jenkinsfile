
def getCommitSha(){
    return sh(returnStdout: true, script: 'git rev-parse --short HEAD')
}

stage('Compile') {
	sh "rm -rf target/*"
	checkout scm
	sh "docker run -it -v \"$(pwd)\":/application docker.sweng.au.dk/haskell865nodeelm:latest /bin/bash -c 'cd application && ./make.sh 'none''"

}

stage('Docker build') {
	sh "docker build . -t docker.sweng.au.dk/hsbefmi:$GIT_COMMIT"
}

stage('Docker push') {
}
