#!/usr/bin/env bash

VERSION=0.0.1-SNAPSHOT
VERSIONED=bartholomews/keyaki:${VERSION}
LATEST=bartholomews/keyaki:latest

docker tag bartholomews/keyaki ${VERSIONED}
echo pushing ${VERSIONED} to registry
docker push ${VERSIONED}
docker tag bartholomews/keyaki ${LATEST}
echo pushing ${LATEST} to registry
docker push ${LATEST}