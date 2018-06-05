#!/bin/bash
##
## A script that builds a container from a given dockerfile, rooted in the
## caller directory
## Arguments: target version
##   Example: nest 2.14.0
##

TARGET=$1
VERSION=$2

# Test args
if [ -z "$TARGET" ] || [ -z "$VERSION" ] ; then
  echo "Two arguments required: target version"
  exit -1
fi

DOCKERTAG="volr/myelin-$TARGET-$VERSION"
DOCKERFILE="ci/$TARGET/Dockerfile-$VERSION"

# Test existence of Dockerfile
if [ ! -f "$DOCKERFILE" ] ; then
  echo "No Dockerfile found: $DOCKERFILE"
  exit -1
fi

# Build
docker build -t "$DOCKERTAG" -f "$DOCKERFILE" `pwd`
# Test
docker run -it "$DOCKERTAG" -i "/myelin/test/simple.json"

# Test dockerlogin
echo "Pushing to Docker"
if [ -z "$DOCKER_PASSWORD" ] || [ -z "$DOCKER_USERNAME" ] ; then
  echo "  No Docker username or password given, terminating"
  exit 0
fi

# Push
echo "$DOCKER_PASSWORD" | docker login --username "$DOCKER_USERNAME" --password-stdin
docker push "$DOCKERTAG"
