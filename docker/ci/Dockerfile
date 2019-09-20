FROM fpco/stack-build-small:lts-14.0

WORKDIR /opt/server
RUN stack update
COPY stack.yaml /opt
COPY server/package.yaml /opt/server

RUN stack build --test --coverage
