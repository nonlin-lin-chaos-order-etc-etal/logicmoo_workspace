#!/bin/bash


apt update
apt install -y iputils-ping


export SHARED_SERVER=10.0.0.194

if ping -c 1 -W 1 "$SHARED_SERVER"; then
   echo "$SHARED_SERVER is UP .. trying to mount..."
   apt install -y nfs-common
   service rpcbind start
   service nfs-common start
   mkdir -p /opt/logicmoo_workspace
   mount -v $SHARED_SERVER:/opt/logicmoo_workspace /opt/logicmoo_workspace
else
   echo "$SHARED_SERVER is pining for the fjords"

   # check out our repo
   if [[ ! -d /opt/logicmoo_workspace ]]
   then
    cd /opt
    git config --global http.sslVerify false \
    git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace 
   else
    cd /opt/logicmoo_workspace
    git checkout master .
   fi
fi

. /opt/logicmoo_workspace/INSTALL.md

#find /opt/logicmoo_workspace/packs_*/ -name "*.qlf" -delete
#rm -f /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/tt0_00022_cycl.qlf
#rm -f /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/plkb0988_kb.qlf

find /opt/logicmoo_workspace/ -type d -exec chmod 777 {} +
chmod a+w -R /opt/logicmoo_workspace/
chmod a+w -R /tmp/

# clearup
#PASSWORD=
#HTTP_PASSWORD=

if [[ -f /startup.sh ]]
then
 /startup.sh &
while :
do
   set -e
   echo "MAYBE (IN OTHER TERMINAL): docker exec -it logicmoo screen -rx"
   echo "OR (IN OTHER TERMINAL): docker exec -it logicmoo bash"
	sleep 30
done
else
 supervisord  -c /etc/supervisor/supervisord.conf
 /opt/logicmoo_workspace/StartLogicmoo.sh
fi

