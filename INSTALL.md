#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

mkdir -p /usr/share/man/man1

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
DIR0=/opt/logicmoo_workspace

(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

#git submodule init
#git submodule update
#git submodule sync --recursive
git config --global http.sslVerify false
#git status -v --show-stash
git pull -f && git pull --recurse-submodules

DIR="$LOGICMOO_WS/lib/deps_installed"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: Logicmoo Deps are hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
else
(source ./INSTALL-DEPS.md )
fi

DIR="$LOGICMOO_WS/swipl-devel"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: SWI-Prolog is hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
else
(source ./INSTALL-SWI.md )
fi


stty sane

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

find -name "*.qlf" -exec touch '{}' +


)
