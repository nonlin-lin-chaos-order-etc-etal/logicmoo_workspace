#!/bin/bash

set +x +e

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

#find $LOGICMOO_WS/?*/ -type d -exec chmod 777 "{}" + 
#chmod a+w -R $LOGICMOO_WS/?*/

git commit -am "Docker $(date)"
git push github master

(
cd docker
docker build -t logicmoo/logicmoo_starter_image $EXTRA .

echo MAYBE: docker push logicmoo/logicmoo_starter_image
docker push logicmoo/logicmoo_starter_image
)

docker build -t logicmoo/logicmoo_workspace $EXTRA  .

echo MAYBE: docker push logicmoo/logicmoo_workspace
docker push logicmoo/logicmoo_workspace

docker run --name logicmoo --privileged=true --rm -it -p 4123:4123 -p 4000-4004:4000-4004 -p 4100-4104:4100-4104 -p 4022:4022 -p 4080:80 -p 4180:4180 -p 4443:443 -p 3020:3020 -p 4020:3020 $EXTRA logicmoo/logicmoo_workspace:latest

)
