#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi


DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"


cd $DIR0
export LOGICMOO_WS=$DIR0
export LOGICMOO_GAMES=$LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games
. $DIR0/logicmoo_env.sh
(
cd $DIR0
echo whoami=`whoami`
echo PATH=$PATH
echo LOGICMOO_GAMES=$LOGICMOO_GAMES
echo LOGICMOO_WS=$LOGICMOO_WS
echo "127.0.0.1 eggdrop"  >> /etc/hosts      
#for internal testing of the build env          
#echo "10.0.0.90 logicmoo.org"  >> /etc/hosts
#git remote add github https://github.com/logicmoo/logicmoo_workspace.git
#git remote add gitlab https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
git status -s
git config --global http.sslVerify false
#git status -v --show-stash
git submodule update --init > /dev/null 2>&1
git pull -f && git pull --recurse-submodules
git update-index --assume-unchanged $LOGICMOO_WS/packs_sys/eggdrop/conf/P*
git status -s
)


( . $DIR0/INSTALL.md )

chmod 777 /opt/logicmoo_workspace/packs_sys/prologmud_samples/prolog/prologmud_sample_games/.swipl_history

screen -wipe
needs_message_update="1"
while [ 0 -lt 4 ]
do

if pgrep -x "screen" > /dev/nulli="0"
then
  if [ "$needs_message_update" != "0" ]; then
    echo "Screen Already Running"
    needs_message_update="0"
  fi
else
    echo "Screen not running"
    screen -mdS "LogicmooServer"
    needs_message_update="0"
    sleep 2
    screen -S LogicmooServer -p0 -X stuff "$DIR0/LogicmooServerLoop.sh\r"
    sleep 2
fi

if  pgrep -f "LogicmooServerLoop" > /dev/nulli="0"
then
   if [ "$needs_message_update" != "0" ]; then
    echo "Looks good!"
    needs_message_update="0"
   fi
else
    echo "Restarting LogicmooServerLoop"
    needs_message_update="1"
    screen -S LogicmooServer -p0 -X stuff "$DIR0/LogicmooServerLoop.sh\r"
    sleep 2
fi

sleep 30

done

