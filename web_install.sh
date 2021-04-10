#!/bin/bash

# Installs with 
#   source <(curl -sS https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/raw/master/web_install.sh)
#   source <(curl -sS https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh)

if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
else

if git --version &>/dev/null; then
   echo "Found Git"; 
else
echo ""
   echo ""
   echo -e "\e[1;31mERROR Require git but it's not installed.  Aborting. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi

mkdir -p /opt
cd /opt
export SSLWAS=$(git config --global http.sslVerify)
git config --global http.sslVerify false
if [ ! -d "logicmoo_workspace" ]; then
  git clone --recursive https://github.com/logicmoo/logicmoo_workspace.git
fi

cd logicmoo_workspace

(source ./INSTALL.md)
git config --global http.sslVerify $SSLWAS

echo -e "\e[1;32m Type: source ./StartLogicmoo.sh \e[0m"

fi

