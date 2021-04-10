#FROM jgoerzen/debian-base-security:buster
#MAINTAINER John Goerzen <jgoerzen@complete.org>
FROM logicalcontracts/lps.swi
LABEL maintainer = "dmiles@gmail.com"

USER root
RUN apt-get update
RUN apt-get install -y software-properties-common apt-utils curl git
RUN mkdir /opt -p
RUN echo " 10.0.0.90 logicmoo.org  "  > /etc/hosts
RUN curl -o /tmp/web_install.sh https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh
RUN /bin/bash -c "source /tmp/web_install.sh"
CMD ["/bin/sh" "-c" "/opt/logicmoo_workspace/StartLogicmoo.sh"]

