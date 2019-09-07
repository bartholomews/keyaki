# PREREQUISITE: a binary compatible keyaki executable built with `stack install`
FROM ubuntu:18.04
WORKDIR /opt/server
COPY /root/.local/bin/keyaki .
COPY client/dist client/dist
ENTRYPOINT ["./keyaki"]