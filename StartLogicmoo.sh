#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi


DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"


(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

                						 
echo "127.0.0.1 eggdrop"  >> /etc/hosts      
#for internal testing of the build env          
#echo "10.0.0.90 logicmoo.org"  >> /etc/hosts
#git remote add github https://github.com/logicmoo/logicmoo_workspace.git
#git remote add gitlab https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
git submodule update --init > /dev/null 2>&1
git pull -f
git update-index --assume-unchanged $LOGICMOO_WS/packs_sys/eggdrop/conf/P*
git status -s

( . INSTALL.md )

screen -wipe

needs_message_update="1"

# git checkout master . 

while [ 0 -lt 4 ]
do

# git status -s
# git pull --recurse-submodules 
# git status -s


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


)

