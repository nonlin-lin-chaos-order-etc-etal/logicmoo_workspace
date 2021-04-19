FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"


EXPOSE 22
EXPOSE 80
EXPOSE 443
EXPOSE 801
EXPOSE 57575

EXPOSE 3020
EXPOSE 3334

EXPOSE 4100
EXPOSE 4101
EXPOSE 4102
EXPOSE 4103
EXPOSE 4104
EXPOSE 4123
EXPOSE 4125

EXPOSE 4000
EXPOSE 4001
EXPOSE 4002
EXPOSE 4003
EXPOSE 4004
EXPOSE 4023
EXPOSE 4025


ENV HOME /root


# @TODO (something here)
MAINTAINER RUN apt-get update && apt-get install -y \
	libcurl4-openssl-dev \
	libssl-dev \
	libcairo-dev \
	libxml2-dev \
	libudunits2-dev \
	libgeos++-dev \
	libtiff-dev \
	libopenmpi-dev \
        libtinfo-dev \
        libreadline-dev \
	swi-prolog-*

#CMD /opt/logicmoo_workspace/StartLogicmoo.sh

COPY . /opt/logicmoo_workspace
