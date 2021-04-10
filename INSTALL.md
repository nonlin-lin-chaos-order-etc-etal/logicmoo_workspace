#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

#d $LOGICMOO_WS

#git submodule init
#git submodule update
#git submodule sync --recursive
git fetch --recurse-submodules
#git status -v --show-stash

(source ./INSTALL-DEPS.md )
(source ./INSTALL-SWI.md )

)

