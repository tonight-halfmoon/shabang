#!/bin/bash

function send_tcp_message()
{
	local address=$1
	local port=$2
	local message=$3
	exec 3<>/dev/tcp/$address/$port
	echo -e "$message" >&3

}


send_tcp_message $1 $2 $3


## Ref https://gist.github.com/akhin/6fe3987af338f8a55ca31eea0733e480
