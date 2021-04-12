FROM debian:buster
FROM httpd:2.4
LABEL maintainer = "logicmoo@gmail.com"

USER root
RUN apt-get update
RUN apt-get install -y apt-utils software-properties-common curl git wget sudo bash libncurses6
RUN mkdir /opt -p
RUN echo "10.0.0.180 logicmoo.org"  > /etc/hosts
RUN curl -o /tmp/web_install.sh https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh
RUN /bin/bash -c "source /tmp/web_install.sh"
#for internal testing of the build env
# RUN sleep 10000000
#CMD ["/bin/bash" "-c" "/opt/logicmoo_workspace/StartLogicmoo.sh"]

