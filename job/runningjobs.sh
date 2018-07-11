#!/bin/bash

function runningjobs {
 jobs -r | wc -l | sed -e "s/ //g"
}
runningjobs
