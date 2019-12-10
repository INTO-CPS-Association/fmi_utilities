FROM haskell:8.6.5

ENV BASE_URL /

RUN apt-get update && apt-get install file unzip zip openjdk-8-jdk -y && apt-get clean
ADD target /application/
COPY target/frontend/index.html /application/base_index.html
WORKDIR application
CMD sed -e "s#{{ BASE_URL }}#$BASE_URL#g" base_index.html > /application/frontend/index.html && /application/HsBeFMI-exe

EXPOSE 80


