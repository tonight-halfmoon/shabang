#!/bin/sh

curr_dir=`pwd`
sdc_log=$curr_dir/sdc.log
start_docker_containers () {
  if ! docker system info &>/dev/null
  then
    dockerd >>$sdc_log 2>&1 &
    while ! docker system info &>/dev/null
    do
      (( i++ == 0 )) && printf %s ' -- Docker Daemon is starting ...' || printf '.'
      sleep 1
    done
    (( i )) && printf '\n'
  fi
  printf %s ' - Docker system is ready' >&2
  printf %s ' - Attempt to start your containers ...' >&2
  docker start $1 $2 $3 >>$sdc_log 2>&1
  wait
  printf %s ' - Docker Stats '
  docker stats
}

start_docker_containers cassandra3 cassandra2 cassandra1
printf '\n'
exit 0
