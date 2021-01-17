# PREREQUISITE: a binary compatible keyaki executable built with `stack install`
# Image used has to be `ubuntu:18.04` as the executable has been built with `stack-build-small`,
# see https://github.com/commercialhaskell/stackage/tree/master/automated/dockerfiles#readme
FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install libpq-dev -y
WORKDIR /opt
COPY keyaki .
COPY client/dist client/dist
ENV ENV=Production
ENV PG_DATABASE=keyaki
ENV PG_PORT=5432
ENTRYPOINT ["./keyaki"]