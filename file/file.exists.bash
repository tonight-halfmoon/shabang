#!/bin/bash

func ()
{
	echo "$1";
}

func 

func $1
if [[ -e "$1" ]]
then
	read_file("$1")	;
fi
;

function read_file(file)
{
	echo "Implement me to read this file "$file"";
}

exit 0
