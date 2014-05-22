#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR=$THIS_DIR/..


setup_cabal_sandbox () {
  cd $ROOT_DIR
  cabal sandbox list-sources > /dev/null
  if [[ $? = 1 ]]; then
    echo "* Initializing cabal sandbox"
    cabal sandbox init
  fi
  cd - > /dev/null
}

add_testloop_dependency () {
    testloop_dir=$THIS_DIR/../vendor/testloop
    [[ -d $testloop_dir ]] || {
        cd $THIS_DIR/../vendor;
        git clone -b development https://github.com/roman/testloop;
    }
    cd $ROOT_DIR
    list_sources=`cabal sandbox list-sources`
    echo $list_sources | grep 'testloop' > /dev/null
    if [[ $? = 1 ]]; then
      echo "* Adding testloop dependency to sandbox"
      cabal sandbox add-source $testloop_dir
    fi
    cd - > /dev/null
}

setup_cabal_sandbox
add_testloop_dependency
