#!/bin/sh

[ -z $1 ] && find dist-newstyle -type f -name OpenBook -exec cp {} ./ \;
[ -z $1 ] || find dist-newstyle -type f -name OpenBook -exec cp {} $1 \;
