FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"


EXPOSE 22
EXPOSE 80
EXPOSE 443
EXPOSE 3020
EXPOSE 3080
EXPOSE 3100
EXPOSE 3101
EXPOSE 3102
EXPOSE 3103
EXPOSE 3123
EXPOSE 3125
EXPOSE 3200
EXPOSE 3201
EXPOSE 3202
EXPOSE 3203
EXPOSE 3223
EXPOSE 3225
EXPOSE 3334
EXPOSE 3401
EXPOSE 3501
EXPOSE 3904
EXPOSE 4000
EXPOSE 4001
EXPOSE 4002
EXPOSE 4003
EXPOSE 4004
EXPOSE 4005
EXPOSE 4006
EXPOSE 801
EXPOSE 57575

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
