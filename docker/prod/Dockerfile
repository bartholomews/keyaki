# PREREQUISITE: a binary compatible keyaki executable built with `stack install`
# Image used has to be `ubuntu:18.04` as the executable has been built with `stack-build-small:lts-14.0`,
# see https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles
FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install libpq-dev -y
WORKDIR /opt
COPY keyaki .
COPY server/postgresql server/postgresql
COPY server/secrets server/secrets
COPY client/dist client/dist
ENTRYPOINT ["./keyaki"]