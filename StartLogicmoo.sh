#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
else

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

source ./INSTALL.md 

sudo -u prologmud_server -- sh -c "${LOGICMOO_WS}/logicmoo_env.sh ; . ${LOGICMOO_WS}/packs_web/butterfly/bin/activate ; export LOGICMOO_WS=$LOGICMOO_WS ; cd ${LOGICMOO_GAMES} ; ./StartMUD.sh $*"
)

stty sane

fi

