#!/usr/bin/env bash

set -euxo pipefail

DOCKER_CONFIG=${HOME}/.docker/config.json

echo "${GPG_PUB}" > bartholomews.gpg.pub
echo "${GPG_PEM}" > bartholomews.gpg.pem
ls
cat bartholomews.gpg.pub

mkdir ${HOME}/.docker && cd ${HOME}/.docker
touch config.json
echo "{ \"credsStore\": \"pass\" }" > config.json

gpg --import bartholomews.gpg.pub

# https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another
touch ~/.gnupg/gpg.conf && echo "pinentry-mode loopback" >> ~/.gnupg/gpg.conf
gpg --no-tty --passphrase "${GPG_PASSPHRASE}" --import bartholomews.gpg.pem

pass init ${GPG_ID}
docker-credential-pass list

echo "trusted-key ${GPG_ID:(-16)}" > ~/.gnupg/gpg.conf
pass generate docker-credential-helpers/docker-pass-initialized-check

echo "${DOCKER_PASSWORD}" | docker login -u ${DOCKER_USER} --password-stdin