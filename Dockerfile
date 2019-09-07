# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
FROM fpco/stack-build-small:lts-14.0 as build-env

########################################################################################################################
## https://github.com/nodesource/distributions/blob/master/README.md#debinstall
## Using Ubuntu
#RUN curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash -
#RUN sudo apt-get install -y nodejs
#
## https://yarnpkg.com/lang/en/docs/install-ci/
## Install yarn as outlined in (https://yarnpkg.com/lang/en/docs/install/#alternatives-stable)
#RUN curl -o- -L https://yarnpkg.com/install.sh | bash
## Make available in the current terminal
#RUN export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
########################################################################################################################

WORKDIR /opt/server
#RUN cd client && yarn install && yarn run prod
RUN stack update
COPY stack.yaml /opt
COPY server/package.yaml /opt/server
RUN stack install --only-dependencies
COPY server /opt/server
RUN stack install
COPY client/dist /opt/client

FROM ubuntu:18.04 as build-prod
WORKDIR /opt/server
COPY --from=build-env /root/.local/bin/keyaki .
COPY --from=build-env /opt/client client/dist
ENTRYPOINT ["./keyaki"]