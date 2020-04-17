#!/bin/zsh

cp -R -f ./02_homework.html /home/emaballarin/hugo/emaballarin.github.io/public/dsscmirror/smds/hw02/
cp -R -f ./figs /home/emaballarin/hugo/emaballarin.github.io/public/dsscmirror/smds/hw02/

/home/emaballarin/bin/hugo-update-my-site
