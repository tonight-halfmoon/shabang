#!/bin/bash
# broken-link.sh
# Written by Lee bigelow
# Used in ABS Guide wih permission.

#  A pure shell script to find dead symlinks and output them qutoted
#+ so they cna be fed to xargs and dealt with :)
#+ e.g. sh broken-link.sh /somedir /someotherdir|xargs rm
#
#  This, however, is a better method:
#
#  find "somedir" -type 1 -print0|\
#  xargs -r0 file|\
#  grep "broken symbolic"|
#  sed -e 's/^\|: *broken symbolic.*$/"/g'
#
#+ but that would'nt be pure Bash, now would it.
#  Caution: beware the /proc file system and any circular links!
###############################################################


