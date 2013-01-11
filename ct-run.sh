#!/bin/sh
cd `dirname $0`
make
ct_run -spec dirmon_test.spec -pa $PWD/ebin edit $PWD/deps/*/ebin 
#   -s reloader

