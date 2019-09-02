# https://github.com/freebroccolo/docker-haskell/issues/54
# https://dzone.com/articles/shrinking-haskell-docker-images-using-multi-stage
# https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/stack-build/lts-13.20/Dockerfile
FROM fpco/stack-build:lts-13.20 as build-env

WORKDIR /opt/foo

# optionally cache the package index too
RUN cd server
RUN stack update
COPY keyaki.cabal /opt/foo
COPY stack.yaml /opt/foo
RUN stack install --only-dependencies
COPY . /opt/foo
RUN stack install
RUN ls /root/.local/bin
#CMD ["/usr/local/bin/stack", "exec", "keyaki-exe"]

FROM ubuntu:16.04 as build-prod

ARG GHC_VERSION=8.6.5
ARG LTS_SLUG=lts-13.20
ARG PID1_VERSION=0.1.2.0
ARG STACK_VERSION=1.9.3
ARG CUDA_VERSION=10.0
ARG BOOTSTRAP_COMMIT=9f2b7ab95c711794257b059604e80ab9ad3c0c45
ARG DEBIAN_FRONTEND=noninteractive
ARG VARIANT=build

RUN apt-get update && \
    apt-get install -y --no-install-recommends wget netbase ca-certificates && \
    if [ "$VARIANT" = "small" ]; then \
        echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main" >>/etc/apt/sources.list && \
        echo "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main" >>/etc/apt/sources.list && \
        apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 063DAB2BDC0B3F9FCEBC378BFF3AEACEF6F88286 && \
        apt-get update && \
	    apt-get install -y --no-install-recommends \
	    	ghc-$GHC_VERSION ghc-$GHC_VERSION-htmldocs \
			g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg \
            libtinfo-dev; \
    else \
        wget -qO- https://raw.githubusercontent.com/fpco/stackage/$BOOTSTRAP_COMMIT/debian-bootstrap.sh | sed "s/^GHCVER=8.6.3$/GHCVER=$GHC_VERSION/" | bash && \
        # Add g++ version required for building 'double-conversion' \
        # (see https://github.com/commercialhaskell/stack/issues/4470) \
        apt-get install -y g++-7; \
    fi && \
    rm -rf /var/lib/apt/lists/*


# FROM phusion/baseimage as build-prod
WORKDIR /opt/server
COPY --from=build-env /root/.local/bin/keyaki .
CMD ["./keyaki"]