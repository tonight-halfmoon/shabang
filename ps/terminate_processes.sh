#!/bin/bash

e_noargs=85

if [ $# -eq 0  ]	
then
	printf "Usage: `basename $0` <Process/Program Name> \n"
	exit $e_noargs
 else
	printf "Attempt to terminate all %s processes." "$1"
	kill `ps aux|grep $1 |awk '{print $2}'`
fi
