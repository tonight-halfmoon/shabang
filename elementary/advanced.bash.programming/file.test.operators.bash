#!/bin/bash

device0="/dev/sda2"      # /  (root directory)
if [ -b "$device0" ]
then
    echo "$device0 is a block device."
fi

# /dev/sda2 is a block device.


device1="/dev/ttyS1"    # PCMCIA modem card.
if [ -c "$device1" ]
then
    echo "$device1 is a character device."
fi

# /dev/ttyS1 is a character device.

function show_input_type()
{
    [ -p /dev/fd/0 ] && echo PIPE || echo STDIN
}

show_input_type "Input"                   # STDIN
echo  "Input" | show_input_type           # PIPE

# This example courtesy of Carl Anderson.
