#!/bin/bash

set +x +e

apt update
apt install -y iputils-ping

export LOGICMOO_WS=/opt/logicmoo_workspace

export SHARED_SERVER=10.0.0.194
export DO_PULL=1

if ping -c 1 -W 1 "$SHARED_SERVER"; then
   echo "$SHARED_SERVER is UP .. trying to mount..."
   apt install -y nfs-common
   service rpcbind start
   service nfs-common start
   mkdir -p $LOGICMOO_WS
   export mount="/myfilesystem"

   if grep -qs "$LOGICMOO_WS" /proc/mounts; then
     echo "$LOGICMOO_WS already mounted."
     DO_PULL=0
   else
     echo "$LOGICMOO_WS is not mounted."
     mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS
     if [ $? -eq 0 ]; then
      echo "Success mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS !"
      DO_PULL=0
     else
      echo "Something went wrong with the mount..."
      rmdir $LOGICMOO_WS
      mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS -v
     fi
   fi
else
   echo "$SHARED_SERVER is not local"
fi

# check out our repo
if [[ ! -d $LOGICMOO_WS ]]
then
 cd /opt
 git config --global http.sslVerify false \
 git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace
fi

cd $LOGICMOO_WS
if ["$DO_PULL"=="1"]; then git checkout master . ; else echo "Skipping pull" ; fi

. $LOGICMOO_WS/INSTALL.md

#find $LOGICMOO_WS/ -type d -exec chmod 777 {} +
#chmod a+w -R $LOGICMOO_WS/
#chmod a+w -R /tmp/

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
 $LOGICMOO_WS/StartLogicmoo.sh
fi

