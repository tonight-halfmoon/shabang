#!/bin/bash

e_noArgs=85

if [ $# -eq 0 ]
then
	printf 'Usage: `basename $0` <1|0> indicating whether to run git pull or not\n'
	exit $e_noArgs 
else 
	exec ./runner.sh ./services $1 ecomgw-bootstrap ecomgw-frontend-temp
fi
