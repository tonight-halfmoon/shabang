#!/bin/bash

e_baddir=85

var=nonexistent_directory

error()
{
	printf "$@" >&2
	echo
	exit $e_baddir
}

cd $var || error $"Can't cd to %s." "$var"
