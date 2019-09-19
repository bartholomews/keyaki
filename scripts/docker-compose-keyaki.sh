#!/usr/bin/env bash
# WORKING DIRECTORY SHOULD BE TOP LEVEL PROJECT
docker-compose -f ./docker/docker-compose.yml -f ./docker/docker-compose.keyaki.yml up