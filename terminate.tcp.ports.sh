#!/bin/bash

#sudo tcpkill host ´netstat -vanp |grep tcp |grep $1´


# on MAC
sudo kill `netstat -vanp tcp |grep $1 | awk '{print $9}'`

# or sudo kill `netstat -van -- p tcp |grep $1 | awk '{print $9}'`
