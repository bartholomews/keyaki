# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
FROM fpco/stack-build-small:lts-14.0 as build-env
WORKDIR /opt/server
RUN stack update
COPY server/keyaki.cabal /opt/server
COPY server/stack.yaml /opt/server
RUN stack install --only-dependencies
COPY server /opt/server
RUN stack install
COPY client/dist /opt/client

FROM ubuntu:18.04 as build-prod
WORKDIR /opt/server
COPY --from=build-env /root/.local/bin/keyaki .
COPY --from=build-env /opt/client client/dist
ENTRYPOINT ["./keyaki"]