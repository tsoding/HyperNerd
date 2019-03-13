#!/bin/sh

# # For security reasons it's better to set these vars outside of the script
# DEPLOY_TARGET="user@example.org"
# DEPLOY_FOLDER="/home/user/"

set -xe

docker build -t hypernerd .
docker save hypernerd | gzip > hypernerd.tar.gz
scp hypernerd.tar.gz "${DEPLOY_TARGET}:${DEPLOY_FOLDER}"
rm hypernerd.tar.gz
ssh "${DEPLOY_TARGET}" -t "gunzip hypernerd.tar.gz"
ssh "${DEPLOY_TARGET}" -t "docker stop hypernerd-bot; docker rm hypernerd-bot"
ssh "${DEPLOY_TARGET}" -t "docker load -i hypernerd.tar && mkdir -p ${DEPLOY_FOLDER}/hypernerd-state/"
ssh "${DEPLOY_TARGET}" -t "docker create -v ${DEPLOY_FOLDER}/hypernerd-state:/tmp/hypernerd/ --name hypernerd-bot hypernerd"
ssh "${DEPLOY_TARGET}" -t "docker start hypernerd-bot"

# TODO(#502): deploy-docker.sh doesn't not remove older hypernerd images
