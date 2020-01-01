# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build
FROM fpco/stack-build:lts-11.10 as build-env

ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

WORKDIR /opt
COPY client/dist /opt/client
COPY server /opt/server
COPY stack.yaml /opt

RUN stack update
RUN stack install --only-dependencies
# https://docs.haskellstack.org/en/latest/coverage/
# RUN stack install --test --coverage
RUN stack install

#RUN stack --install-ghc test --only-dependencies
#RUN stack build --test --coverage

FROM ubuntu:18.04 as build-prod
RUN apt-get update
RUN apt-get install libpq-dev -y
WORKDIR /opt
COPY --from=build-env /root/.local/bin/keyaki .
COPY --from=build-env /opt/client client/dist
COPY --from=build-env /opt/server/postgresql server/postgresql
COPY --from=build-env /opt/server/secrets server/secrets
ENTRYPOINT ["./keyaki"]
