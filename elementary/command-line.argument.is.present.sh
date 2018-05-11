#!/bin/bash

if [ -n "$1" ]
then
    lines=$1
else
    lines=99
fi

echo $lines


function func()
{
    local lines2=113
    echo $lines2
}

func
: 
