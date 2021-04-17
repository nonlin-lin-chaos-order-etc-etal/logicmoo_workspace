#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
$DIR0/StartLogicmoo.sh

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
    sleep 2
    screen -S LogicmooServer -p0 -X stuff "$DIR0/StartLogicmoo.sh\r"
    sleep 2
    echo "Screen Started"
fi

if  pgrep -f "StartLogicmoo" > /dev/nulli="0"
then
   if [ "$needs_message_update" != "0" ]; then
    echo "Looks like StartLogicmoo is running!"
    needs_message_update="0"
   fi
else
    echo "Starting StartLogicmoo"
    needs_message_update="1"
    screen -S LogicmooServer -p0 -X stuff "$DIR0/StartLogicmoo.sh\r"
    sleep 2
fi

sleep 30

done

