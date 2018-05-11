#!/bin/bash

filename=2del.file

[ -f "$filename" ] &&
    foo(){ rm -f "$filename"; echo "File "$filename" deleted."; } ||
	foo() { echo "File "$filename" not found."; touch bar; }

foo
