# PREREQUISITE: a binary compatible keyaki executable built with `stack install`
# Image used has to be `ubuntu:18.04` is the executable has been built with `stack-build-small:lts-14.0`,
# see https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles
FROM ubuntu:18.04
WORKDIR /opt/server
COPY keyaki .
COPY client/dist client/dist
ENTRYPOINT ["./keyaki"]