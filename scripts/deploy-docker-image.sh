#!/usr/bin/env bash

REPOSITORY=bartholomews/keyaki
SEMVER=0
VERSION=0.0.1
IMAGE=${REPOSITORY}:${VERSION}

docker tag ${IMAGE} ${REPOSITORY}:latest
echo pushing ${IMAGE} to registry
docker push ${IMAGE}
echo pushing ${REPOSITORY}:latest to registry
#docker push ${REPOSITORY}:latest