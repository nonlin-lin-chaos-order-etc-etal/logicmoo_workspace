#FROM jgoerzen/debian-base-security:buster
#MAINTAINER John Goerzen <jgoerzen@complete.org>
#FROM logicalcontracts/lps.swi
FROM debian:buster
LABEL maintainer = "logicmoo@gmail.com"

USER root
RUN apt-get update
RUN apt-get install -y apt-utils software-properties-common curl git wget sudo bash libncurses6
RUN mkdir /opt -p
RUN echo "10.0.0.180 logicmoo.org"  > /etc/hosts
RUN curl -o /tmp/web_install.sh https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh
RUN /bin/bash -c "source /tmp/web_install.sh"
RUN sleep 10000000
CMD ["/bin/sh" "-c" "/opt/logicmoo_workspace/StartLogicmoo.sh"]

