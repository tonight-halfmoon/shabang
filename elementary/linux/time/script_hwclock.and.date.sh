#!/bin/bash  

#foo() {
#    echo $1 start 
    printf "hwclock: $(hwclock)\n" &
    printf "date: $(date)\n" &
#    sleep $(( $1 & 03))
#    echo $1 complete
#}

#echo First :
#foo 1 &

wait 


