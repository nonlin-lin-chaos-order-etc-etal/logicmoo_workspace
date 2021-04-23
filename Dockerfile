FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"

# SSHD
EXPOSE 22    
# Apache HTTP 
EXPOSE 80    
# Apache SSL 
EXPOSE 443 4443
# Ngynx
EXPOSE 4801   
# Butterfly Logins
EXPOSE 4180
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
                           # Non-Shared SWIPL ?-

EXPOSE 4090 4091


ENV HOME /root

COPY docker/rootfs /

RUN echo enable some apache mods \
 && a2dismod mpm_event \
 && a2enmod access_compat alias auth_basic authn_core authn_file authz_core authz_host authz_user autoindex deflate dir env \
 filter headers http2 mime mpm_prefork negotiation  php7.4 proxy proxy_ajp proxy_balancer proxy_connect proxy_express \
 proxy_fcgi proxy_fdpass proxy_ftp proxy_hcheck proxy_html proxy_http proxy_http2 proxy_scgi proxy_uwsgi proxy_wstunnel reqtimeout \
 rewrite setenvif slotmem_shm socache_shmcb ssl status xml2enc ; /bin/true \
 \
# confirm our webconfig works (or it exits docker build) \
 && service apache2 start && service apache2 status

# @TODO (something here)
ENV LOGICMOO_WS /opt/logicmoo_workspace

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

