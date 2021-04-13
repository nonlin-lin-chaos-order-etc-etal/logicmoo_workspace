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

ln -s $LOGICMOO_WS/etc/profile.d/logicmoo_etc_profile_d.sh /etc/profile.d/ > /dev/null 2>&1
ln -s $LOGICMOO_WS/packs_web/logicmoo_webui/etc/apache2/sites-enabled/000-logicmoo.conf /etc/apache2/sites-enabled/000-logicmoo.conf > /dev/null 2>&1
ln -s $LOGICMOO_WS/packs_web/logicmoo_webui/etc/apache2/conf-available/cliopatria_swish.conf /etc/apache2/conf-available/cliopatria_swish.conf > /dev/null 2>&1

echo adduser --disabled-password --gecos "" --no-create-home prologmud_server --home $LOGICMOO_GAMES
adduser --disabled-password --gecos "" --no-create-home prologmud_server --home $LOGICMOO_GAMES

chown prologmud_server $LOGICMOO_GAMES
chown -R prologmud_server $LOGICMOO_GAMES/*??*

cp -f $LOGICMOO_GAMES/.??*rc ~/
cp -f $LOGICMOO_GAMES/.bash* ~/
cp -f $LOGICMOO_GAMES/.profile* ~/

touch $LOGICMOO_GAMES/history_3804
touch $LOGICMOO_GAMES/completion_3804
chown prologmud_server $LOGICMOO_GAMES/completion_*
chown prologmud_server $LOGICMOO_GAMES/history_*

touch $LOGICMOO_GAMES/nohup.out
chown prologmud_server $LOGICMOO_GAMES/nohup.out
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/eggdrop/
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/

# in case of symlinking
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/
chown -R prologmud_server /opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/pldata/plkb0988/src~/

#chown -R prologmud_server $LOGICMOO_WS/packs_web/butterfly

mkdir -p /tmp/tempDir/
chown -R prologmud_server /tmp/tempDir/
)
