#!/bin/bash
echo -n $1 | shasum -a 256 | awk '{print $1}'

