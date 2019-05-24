FROM ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y \
&& apt-get install -y locales

RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN apt-get install -y libgmp10 netbase ca-certificates \
    && apt-get clean all

RUN update-ca-certificates
RUN apt-get install -y apt-utils make
RUN mkdir -p /tmp
COPY docker.mk /tmp

WORKDIR /tmp
RUN make -f docker.mk init

ENV DEBIAN_FRONTEND=interactive
