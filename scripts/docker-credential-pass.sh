#!/usr/bin/env bash

set -euxo pipefail

DOCKER_CONFIG=${HOME}/.docker/config.json

gpg --import docker/GPG_PUB

# https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another
touch ~/.gnupg/gpg.conf && echo "pinentry-mode loopback" >> ~/.gnupg/gpg.conf
gpg --no-tty --passphrase "${GPG_PASSPHRASE}" --import "${GPG_PEM}"

mkdir ${HOME}/.docker && touch ${HOME}/.docker/config.json
echo "{ \"credsStore\": \"pass\" }" > ${HOME}/.docker/config.json

pass init ${GPG_ID}

echo "trusted-key ${GPG_ID:(-16)}" > ~/.gnupg/gpg.conf
pass generate docker-credential-helpers/docker-pass-initialized-check

echo "${DOCKER_PASSWORD}" | docker login -u ${DOCKER_USER} --password-stdin