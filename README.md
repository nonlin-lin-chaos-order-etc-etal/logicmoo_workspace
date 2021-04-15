# logicmoo's LogicMOO Parent Project 

Initial starter Docs https://github.com/logicmoo/logicmoo_workspace/wiki

As well as https://docs.google.com/document/d/1fkOxnmI1LqxadvZuCRS-fGIEweIKyPn6AVGp5Yjse1I/edit

=========

# Install and Run

```bash
cd /opt
git clone --recursive https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
cd logicmoo_workspace
source ./INSTALL.md
./StartLogicmoo.sh

```

# Your Docker 

```docker build - --no-cache < Dockerfile```



# Douglas' Docker 

```
docker build -t logicmoo/logicmoo_starter_image:latest  --no-cache --add-host=logicmoo.org:10.0.0.90 --file Dockerfile.distro .   
docker push logicmoo/logicmoo_starter_image:latest
docker-compose build --add-host=logicmoo.org:10.0.0.90

docker network create -d macvlan --subnet=10.0.0.0/24 --gateway=10.0.0.1 -o parent=eth0 pub_net

docker run -t --add-host=logicmoo.org:10.0.0.90  --network="logicmoo_workspace_prologmud_vlan"  logicmoo/logicmoo_starter_image:latest

docker exec -it $(docker ps -n 1 -q) bash


docker build -t logicmoo/logicmoo_workspace:latest --no-cache --add-host=logicmoo.org:10.0.0.90 - < Dockerfile
docker push logicmoo/logicmoo_workspace:latest 
```



```
docker kill $(docker ps -a -q)
docker image prune --all -f
docker rmi logicmoo/logicmoo_starter_image:latest 
```
