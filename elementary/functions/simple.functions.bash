#!/bin/bash

JUST_A_SECOND=1

fun() { echo "This is a function";  }

funky()
{
    echo "This is a funky function."
    echo "Now existing funky function."
}

fun2()
{
    i=0
    REPEATS=30

    echo
    echo "And now the fun really begins."
    echo

    sleep $JUST_A_SECOND
    while [ $i -lt $REPEATS ]
    do
	echo "------------Functions----------"
	echo "---------------ARE----------"
	echo "---------------FUN----------"
	echo
	let "i+=1"
    done
}

fun
funky
fun2

exit $?
