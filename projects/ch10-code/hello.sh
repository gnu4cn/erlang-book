#! /usr/bin/env bash

erl -noshell -pa $HOME/erlang-book/projects/ch10-code -s hello start -s init stop
