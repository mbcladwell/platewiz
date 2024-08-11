#! /bin/bash

PATH_INTO_STORE=abcdefgh
export LC_ALL="C"
echo export LC_ALL=\"C\" >> $HOME/.bashrc
echo export GUIX_PROFILE=$HOME/.guix-profile >> $HOME/.bashrc
echo . $HOME/.guix-profile/etc/profile >> $HOME/.bashrc
echo export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale >> $HOME/.bashrc

mkdir -p $HOME/.config/platewiz
cp $PATH_INTO_STORE/share/guile/site/3.0/platewiz/conf/artanis.conf $HOME/.config/platewiz
## sudo chmod u+w $HOME/.config/platewiz/artanis.conf
