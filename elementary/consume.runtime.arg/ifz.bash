#!/bin/bash

E_NOARGS=85
E_NOTFOUND=86
E_NOTGZIP=87

#if [ -z "$1" ]
if [ $# -eq 2 ] 
then
    echo "Usage: `basename $0` filename" >&2
    exit $E_NOARGS
fi

filename=$1
if [ ! -f "$filename" ]
then
    echo "Fie $filename not found!" >&2
    exit $E_NOTFOUND
fi

if [ ${filename##*.} != "gz" ]
then
    echo "File $1 is not a gz file!"
    exit $E_NOTGZIP
fi

zcat $1 | more

exit $?
