#!/bin/bash

# check out our repo
mkdir -p /opt \
 && cd /opt \
 && git config --global http.sslVerify false \
 ; git clone https://github.com/logicmoo/logicmoo_workspace 

/opt/logicmoo_workspace/INSTALL.md

# clearup
#PASSWORD=
#HTTP_PASSWORD=

/startup.sh
#exec /bin/tini -- supervisord -n -c /etc/supervisor/supervisord.conf
