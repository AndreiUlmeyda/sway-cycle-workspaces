#!/bin/bash

# Download a few libraries used for bash unit testing

echo "Cloning testing dependencies from github. The resulting 'lib' directory is not cleaned up automatically."

git clone https://github.com/sstephenson/bats lib/bats
git clone https://github.com/ztombol/bats-support lib/bats-support
git clone https://github.com/ztombol/bats-file lib/bats-file
git clone https://github.com/ztombol/bats-assert lib/bats-assert
git clone https://github.com/jasonkarns/bats-mock lib/bats-mock