#!/bin/bash

# check out our repo
if [[ ! -d /opt/logicmoo_workspace ]]
then
 mkdir -p /opt
 cd /opt 
 git config --global http.sslVerify false \
 git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace 
fi

cd /opt/logicmoo_workspace
git checkout master .

. /opt/logicmoo_workspace/INSTALL.md

find /opt/logicmoo_workspace/packs_sys/logicmoo_nlu -name "*.qlf" -delete
rm -rf /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/tt0_00022_cycl.qlf

find /opt/logicmoo_workspace/ -type d -exec chmod 777 {} +
chmod a+w -R /opt/logicmoo_workspace/
chmod a+w -R /tmp/

# clearup
#PASSWORD=
#HTTP_PASSWORD=

if [[ -f /startup.sh ]]
then
 /startup.sh &
else
 supervisord  -c /etc/supervisor/supervisord.conf
 /opt/logicmoo_workspace/StartLogicmoo.sh
fi


sleep 10000000000000