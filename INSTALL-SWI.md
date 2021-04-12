#!/bin/bash

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd $DIR0
export LOGICMOO_WS=$DIR0

./logicmoo_env.sh .

DIR="$LOGICMOO_WS/swipl-devel"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: SWI-Prolog is hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
    return 0 2>/dev/null
    exit 0


fi

echo "#* "
echo "#* Install deps..."
echo "#* "

apt-add-repository -y ppa:swi-prolog/devel
apt-get -y install cmake ninja-build $(apt-cache depends swi-prolog | grep Depends | sed "s/.*ends:\ //" | tr '\n' ' ')
#apt-get build-dep swi-prolog
apt-get install -y \
        build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre3-dev \
        libyaml-dev 
        # default-jdk junit4
  
(
cd $LOGICMOO_WS

echo "#* "
echo "#* Installing swipl in ${DIR}..."
echo "#* "

#mkdir -p bin/
#mkdir .local/share/swi-prolog/pack -p
#chmod 555 .local/share/swi-prolog/pack
find packs_* -name "*.qlf" -delete

git clone https://github.com/SWI-Prolog/swipl-devel.git swipl-devel

(cd swipl-devel/
git reset --hard HEAD
git clean -f -x 
git checkout origin/master . -f
git submodule update --init
git pull --recurse-submodules
patch -p1 --merge < /opt/logicmoo_workspace/dmiles-attvar.patch
git status)

(cd swipl-devel
 mkdir -p build
 cd build
 cmake -DCMAKE_INSTALL_PREFIX=$LOGICMOO_WS -G "Unix Makefiles" ..
 make
 ctest -j 8
 make install)

)

echo "#* MAYBE cat .swiplrc >> ~/.config/swi-prolog/init.pl"
stty sane


