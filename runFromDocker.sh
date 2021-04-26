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

#find $LOGICMOO_WS/?*/ -type d -exec chmod 777 "{}" + 
#chmod a+w -R $LOGICMOO_WS/?*/

echo "Scanning changes for GIT ..."
git status -s

if [[ "${1}"=="commit" ]]; 
 then
   git submodule foreach 'git commit -am "Docker $(date)" ; git push ; SUBM=$(basename `pwd`) ; echo $SUBM  ; cd .. ; git add $SUBM  ; /bin/true'
   git commit -am "Docker $(date)"
   git push github master
   shift 1
fi

EXTRA="${*}"
if ping -c 1 -W 1 "10.0.0.194"; then
  EXTRA+=" --add-host logicmoo.org:10.0.0.194"
fi


(
cd docker
docker build $EXTRA -t logicmoo/logicmoo_starter_image .

echo MAYBE: docker push logicmoo/logicmoo_starter_image
docker push logicmoo/logicmoo_starter_image &
)

docker build $EXTRA -t logicmoo/logicmoo_workspace .

echo MAYBE: docker push logicmoo/logicmoo_workspace
docker push logicmoo/logicmoo_workspace &

docker kill logicmoo ; /bin/true

export DOCKER_RUN="--name logicmoo --privileged=true -v /opt/logicmoo_workspace:/opt/logicmoo_workspace --rm -it -p 4000-4440:4000-4440 -p 4443:443 -p 3020:3020 $EXTRA logicmoo/logicmoo_workspace:latest"

echo "docker run $DOCKER_RUN"
docker run $DOCKER_RUN

)
