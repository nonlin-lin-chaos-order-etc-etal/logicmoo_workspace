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

screen -wipe

# source ./INSTALL.md 

function xscreen {
    # Usage: xscreen <screen-name> command...
    local SCREEN_NAME=$1
    shift

    # Create screen if it doesn't exist
    if ! screen -list | grep $SCREEN_NAME >/dev/null ; then
        screen -dmS $SCREEN_NAME
    fi

    # Create I/O pipes
    local DIR=$( mktemp -d )
    local STDIN=$DIR/stdin
    local STDOUT=$DIR/stdout
    local STDERR=$DIR/stderr
    mkfifo $STDIN $STDOUT $STDERR
    trap 'rm -f $STDIN $STDOUT $STDERR; rmdir $DIR' RETURN

    # Print output and kill stdin when both pipes are closed
    { cat $STDERR >&2 & cat $STDOUT & wait ; fuser -s -PIPE -k -w $STDIN ; } &

    # Start the command (Clear line ^A^K, enter command with redirects, run with ^O)
    screen -S $SCREEN_NAME -p0 -X stuff "$(echo -ne '\001\013') { $* ; } <$STDIN 1> >(tee $STDOUT) 2> >(tee $STDERR >&2)$(echo -ne '\015')"

    # Forward stdin
    cat > $STDIN

    # Just in case stdin is closed
    wait
}

local needs_message_update=1

while [ 0 -lt 4 ]
do

git checkout .
git pull --recurse-submodules
git status


if pgrep -x "screen" > /dev/nulli="0"
then
  if [ $needs_message_update -ne 0 ]; then
    echo "Screen Already Running"
    needs_message_update=0
  fi
else
    echo "Screen not running"
    screen -mdS "LogicmooServer"
    needs_message_update=0
    sleep 2
    screen -S LogicmooServer -p0 -X stuff "$DIR0/LogicmooServerLoop.sh\r"
    sleep 2
fi

if  pgrep -f "LogicmooServerLoop" > /dev/nulli="0"
then
   if [ $needs_message_update -ne 0 ]; then
    echo "Looks good!"
    needs_message_update=0
   fi
else
    echo "Restarting LogicmooServerLoop"
    needs_message_update=1
    screen -S LogicmooServer -p0 -X stuff "$DIR0/LogicmooServerLoop.sh\r"
    sleep 2
fi

sleep 10

done


)

