#!/usr/bin/env bash

for i in `ls *tcl | grep syntax`; do ./bubbleToPostscript.tcl < $i > $(basename $i .tcl).ps; done

