#!/usr/bin/env bash

for i in `ls *tcl | grep syntax`; do ./bubbleToPostscript.tcl < $i > $i.ps; done

