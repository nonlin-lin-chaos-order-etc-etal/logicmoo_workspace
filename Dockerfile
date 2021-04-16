# On Logicmoo's LAN: 
#
#  docker build -t logicmoo/logicmoo_workspace  --add-host=logicmoo.org:10.0.0.90 - < ./Dockerfile
# 
# Elsewhere
#
#  docker build -t logicmoo/logicmoo_workspace - < ./Dockerfile
#
FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"

# Set environment variables
ARG LOGICMOO_EXTRAS
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV SHELL /bin/bash


# who/where
ENV LOGICMOO_USER prologmud_server
ENV LOGICMOO_WS /opt/logicmoo_workspace
ENV LOGICMOO_GAMES $LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games

RUN if [ ! -z "$LOGICMOO_EXTRAS" ]; then curl -O http://mirror.umd.edu/eclipse/technology/epp/downloads/release/2020-06/R/eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz \
  && tar -zxvf eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz -C /usr/ \
  && ln -s /usr/eclipse/eclipse /usr/bin/eclipse ; fi


WORKDIR $LOGICMOO_WS
# Pull in fixes
RUN git fetch origin \
 && git reset --hard origin/master \
 && git pull --recurse-submodules

# do local updates
# RUN ./INSTALL.md

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

WORKDIR /opt/logicmoo_workspace/.
ENTRYPOINT /opt/logicmoo_workspace/StartLogicmoo.sh

