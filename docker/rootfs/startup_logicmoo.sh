#!/bin/bash

# check out our repo
mkdir -p /opt \
 && cd /opt \
 && git config --global http.sslVerify false \
 ; git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace 

find /opt/logicmoo_workspace/ -type d -exec chmod 777 {} +
chmod a+w -R /opt/logicmoo_workspace/
chmod a+w -R /tmp/

. /opt/logicmoo_workspace/INSTALL.md


# clearup
#PASSWORD=
#HTTP_PASSWORD=

# [[ -f /startup.sh ]] && /startup.sh

#(echo exec /bin/tini -- supervisord -n -c /etc/supervisor/supervisord.conf ; /bin/true)

/opt/logicmoo_workspace/StartLogicmoo.sh

sleep 10000000000000
