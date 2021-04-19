#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
DIR0=/opt/logicmoo_workspace

(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

EXTRA="${@}"
EXTRA="$EXTRA --add-host logicmoo.org:10.0.0.194"

git commit -am docker/ros-eloquent-desktop.sh
git push gitlab

(
cd docker
docker build -t logicmoo/logicmoo_starter_image $EXTRA .

echo MAYBE: docker push logicmoo/logicmoo_starter_image
docker push logicmoo/logicmoo_starter_image
)

docker build -t logicmoo/logicmoo_workspace $EXTRA  .

echo MAYBE: docker push logicmoo/logicmoo_workspace

exec docker run --name logicmoo -ti --rm -p 4022:57575 -p 4123:4123 -p 4180:80/tcp -p 4443:443/tcp -p 4020:3020/tcp -p 4000-4004:4000-4004/tcp -p 4100-4104:4100-4104/tcp -p 4188:801 $EXTRA logicmoo/logicmoo_workspace:latest


)
