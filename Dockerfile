FROM adoptopenjdk/openjdk8-openj9:alpine-slim

ARG SBT_VERSION=1.3.13

RUN apk --no-cache add bash ncurses

RUN apk --no-cache --virtual .build-deps add curl \
    && curl -sSL https://piccolo.link/sbt-${SBT_VERSION}.tgz | tar -xvz -C /usr/local \
    && ln -s /usr/local/sbt/bin/sbt /usr/local/bin/sbt \
    && apk del .build-deps
