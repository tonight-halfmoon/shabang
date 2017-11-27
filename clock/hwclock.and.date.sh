#!/bin/bash
printf "hwclock: $(hwclock)\n" &
printf "date: $(date)\n" &
wait

