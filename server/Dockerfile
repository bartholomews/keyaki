# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
FROM fpco/stack-build-small:lts-14.0 as build-env
# FROM fpco/stack-build:lts-14.0 as build-env

WORKDIR /opt/foo

# optionally cache the package index too
RUN stack update
COPY keyaki.cabal /opt/foo
COPY stack.yaml /opt/foo
RUN stack install --only-dependencies
COPY . /opt/foo
RUN stack install
RUN ls /root/.local/bin

FROM ubuntu:18.04 as build-prod
WORKDIR /opt/server
COPY --from=build-env /root/.local/bin/keyaki .
ENTRYPOINT ["./keyaki"]