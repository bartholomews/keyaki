# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
FROM fpco/stack-build-small:lts-14.0 as build-env

RUN apt-get update && apt-get install libpq-dev -y

WORKDIR /opt/server
COPY client/dist /opt/client
RUN stack update
COPY stack.yaml /opt
COPY server/package.yaml /opt/server
RUN stack install --only-dependencies
COPY server /opt/server
RUN stack install

FROM ubuntu:18.04 as build-prod
WORKDIR /opt/server
COPY --from=build-env /root/.local/bin/keyaki .
COPY --from=build-env /opt/client client/dist
ENTRYPOINT ["./keyaki"]
