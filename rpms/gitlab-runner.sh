#!/bin/sh

arch=amd64

OLD_PWD=$PWD
LOCAL_PKG_DIR=$HOME/.local/pkg
if [ ! -d ${HOME_PKG_DIR} ]; then
    mkdir -pv ${HOME_PKG_DIR}
fi

cd ${HOME_PKG_DIR}
# A full list of architectures can be found here https://s3.dualstack.us-east-1.amazonaws.com/gitlab-runner-downloads/latest/index.html
curl -LJO "https://s3.dualstack.us-east-1.amazonaws.com/gitlab-runner-downloads/latest/rpm/gitlab-runner-helper-images.rpm"
curl -LJO "https://s3.dualstack.us-east-1.amazonaws.com/gitlab-runner-downloads/latest/rpm/gitlab-runner_${arch}.rpm"

dnf -y gitlab-runner-helper-images.rpm gitlab-runner_${arch}.rpm

cd ${OLD_PWD}
