FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"


# SSHD
EXPOSE 22    
# Apache HTTP 
EXPOSE 80    
# Apache SSL 
EXPOSE 443   
# Ngynx
EXPOSE 801   
# Butterfly Logins
EXPOSE 57575
# SWISH
EXPOSE 3020
# Eggdrop
EXPOSE 3334

#      HTTPS  HTTP  TELNET
EXPOSE 14100  4100  4000   
                            # MUD Plain Text
EXPOSE 14101  4101  4001   
                            # MUD with Debug
EXPOSE 14102  4102  4002  
                            # MUD with Graphics
EXPOSE 14103  4103  4003  
                           # WAM-CL REPL
EXPOSE 14104  4104  4004  
                           # NOMIC MU
EXPOSE 14123  4123  4023  
                           #  Shared SWIPL ?-
EXPOSE 14125  4125  4025   
                          // # Non-Shared SWIPL ?-

#EXPOSE 4100
#EXPOSE 4101
#EXPOSE 4102
#EXPOSE 4103
#EXPOSE 4104
#EXPOSE 4123
#EXPOSE 4125

#EXPOSE 4000
#EXPOSE 4001
#EXPOSE 4002
#EXPOSE 4003
#EXPOSE 4004
#EXPOSE 4023
#EXPOSE 4025


ENV HOME /root


# @TODO (something here)
ENV LOGICMOO_WS /opt/logicmoo_workspace

COPY . $LOGICMOO_WS
ENV PATH="${LOGICMOO_WS}/bin:${PATH}"
ENV WNDB $LOGICMOO_WS/packs_sys/logicmoo_nlu/data/WNprolog-3.0/prolog

MAINTAINER RUN cd $LOGICMOO_WS && set -x \
 && git checkout . \
 && git submodule update --init . \
 && cd $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata && swipl -g "time(qcompile(wn_iface)),halt." \
 && cd $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata && swipl -g "time(qcompile(tt0_00022_cycl)),halt." \
 \
 && cd $LOGICMOO_WS/packs_xtra/logicmoo_pldata \
 && git checkout . \
 && git checkout master \
 && cd $LOGICMOO_WS/packs_xtra/logicmoo_pldata/ext/plkb0988 \
 && swipl -g "time(qcompile(plkb0988_kb)),halt." \
 && git status \
 && git add -f plkb0988_kb.qlf \
 && cd $LOGICMOO_WS/packs_xtra/ \
 && git add -f . \
 && git commit -am "plkb0988-$(date)" \
 && cd $LOGICMOO_WS/packs_xtra/ \
 && git add logicmoo_pldata \
 && git commit -am "logicmoo_pldata-$(date)" \
 && rm -rf $LOGICMOO_WS/packs_xtra/logicmoo_pldata/*/

#CMD $LOGICMOO_WS/StartLogicmoo.sh

