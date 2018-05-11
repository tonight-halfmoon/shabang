#!/bin/bash
function nprocesses {
	ps ax | wc -l | sed -e "s: ::g"
}

nprocesses
