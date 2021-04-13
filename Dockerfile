FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"

EXPOSE 22
EXPOSE 80
EXPOSE 4000
EXPOSE 4001
EXPOSE 4002
EXPOSE 4003
EXPOSE 4004
EXPOSE 4005
EXPOSE 4006
EXPOSE 3334
EXPOSE 3020
EXPOSE 3080

# @TODO (something here)


CMD /opt/logicmoo_workspace/StartLogicmoo.sh

