#!/bin/bash
ifconfig en0 | grep inet | cut -d: -f 4 | awk '{print $2}'