#!/bin/sh
lsof -ni|grep TCP|grep -v LISTEN|awk '{print $9}'|cut -d: -f 4 | sort | uniq
