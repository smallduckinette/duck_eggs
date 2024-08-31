#!/bin/bash

ROOT=`dirname "$0"`

docker stop dev
docker rm dev
docker build -t devenv "$ROOT"
docker run -v ~/.emacs:/root/.emacs -v "$ROOT":/src -v /dev/input:/dev/input --privileged -d -ti --network host --name dev devenv /bin/bash
