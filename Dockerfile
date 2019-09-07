FROM docker:19.03.2
RUN ls
RUN apk update && apk add bash
ADD scripts/find-version.sh deploy.sh
RUN ./deploy.sh