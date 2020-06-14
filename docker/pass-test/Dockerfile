FROM docker:19.03.1

COPY GPG_PEM .
COPY GPG_PUB .

ENV DOCKER_PASSWORD='???'

ENV DOCKER_PASS=docker-credential-pass-v0.6.3-amd64.tar.gz
ENV PASS=password-store-1.7.3

ENV GPG_ID='???'
ENV GPG_LONG_KEY='???'
ENV GPG_PASSPHRASE='???'

########################################################################################################################
# Fetch and install `pass` and `docker-credential-pass`
########################################################################################################################
RUN apk add --update bash python3 libarchive-tools make duplicity xclip
RUN wget https://github.com/docker/docker-credential-helpers/releases/download/v0.6.3/${DOCKER_PASS}
RUN tar -xf $DOCKER_PASS
RUN wget https://git.zx2c4.com/password-store/snapshot/$PASS.tar.xz
RUN bsdtar xfJ $PASS.tar.xz
RUN cd $PASS && make install
RUN mv docker-credential-pass /usr/bin
RUN chmod 100 /usr/bin/docker-credential-pass
########################################################################################################################

ENV DOCKER_CONFIG=${HOME}/.docker/config.json
RUN mkdir ${HOME}/.docker && cd ${HOME}/.docker
RUN touch config.json
RUN echo "{ \"credsStore\": \"pass\" }" > config.json

RUN gpg --import GPG_PUB

RUN touch ~/.gnupg/gpg.conf
RUN echo "pinentry-mode loopback" >> ~/.gnupg/gpg.conf
RUN cat ~/.gnupg/gpg.conf

# https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another
RUN gpg --no-tty --passphrase "$GPG_PASSPHRASE" --import GPG_PEM

RUN pass init $GPG_ID

RUN echo "trusted-key $GPG_LONG_KEY" > ~/.gnupg/gpg.conf
#RUN pass generate -c docker-credential-helpers/docker-pass-initialized-check

RUN echo "${DOCKER_PASSWORD}" | docker login -u bartholomews --password-stdin