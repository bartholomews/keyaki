#!/bin/bash

# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
docker build -t bartholomews/keyaki \
#    --build-arg random=${RANDOM} \
    --target build-prod \
    -f docker/local/Dockerfile .
