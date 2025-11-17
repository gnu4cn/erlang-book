#! /usr/bin/env sh
erl -boot /home/hector/erlang-book/projects/appendix/see\
    -environment `printenv` -load $1
