#!/usr/bin/env bash

DOCKER_PASS=docker-credential-pass-v0.6.3-amd64.tar.gz
PASS=password-store-1.7.3

########################################################################################################################
# Fetch and install `pass` and `docker-credential-pass`
########################################################################################################################
apk add --update libarchive-tools make bash duplicity
wget https://github.com/docker/docker-credential-helpers/releases/download/v0.6.3/${DOCKER_PASS}
tar -xf ${DOCKER_PASS}
wget https://git.zx2c4.com/password-store/snapshot/${PASS}.tar.xz
bsdtar xfJ ${PASS}.tar.xz
cd ${PASS} && make install
mv docker-credential-pass /usr/bin
chmod 100 /usr/bin/docker-credential-pass
########################################################################################################################

DOCKER_CONFIG=${HOME}/.docker/config.json

echo "${GPG_PUB}" > "$(pwd)/bartholomews.gpg.pub"
echo "${GPG_PEM}" > "$(pwd)/bartholomews.gpg.pem"

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