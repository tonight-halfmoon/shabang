check () {
    while true
    do
        clear && docker ps -a
        wait && sleep 1 && (( i++ == 0 )) && printf %s ' -- checking docker containers ..' && sleep 1
        (( i++ ))
        (( i++ ))
        while [ $i -gt 0 ]
        do
                #printf '.' && i=`expr $i - 1` && sleep 2
                printf '.' && (( i-- )) && sleep 2
        done
        i=0
    done
}

check
exit 0
