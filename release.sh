#!/bin/bash

REPOSITORY=bartholomews/keyaki
SEMVER=0
VERSION=0.0.1
IMAGE=${REPOSITORY}:${VERSION}

# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
docker build -t keyaki --target build-prod .
docker tag ${IMAGE} ${REPOSITORY}:latest
echo pushing ${IMAGE} to registry
docker push ${IMAGE}
echo pushing ${REPOSITORY}:latest to registry
docker push ${REPOSITORY}:latest