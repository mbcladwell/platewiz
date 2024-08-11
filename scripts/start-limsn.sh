#!/bin/bash
PATH_INTO_STORE=abcdefgh
export LC_ALL="C"
mkdir -p /var/tmp/platewiz/tmp/cache
cd $PATH_INTO_STORE/share/guile/site/3.0/platewiz
art work -h0.0.0.0 --config=$HOME/.config/platewiz/artanis.conf
