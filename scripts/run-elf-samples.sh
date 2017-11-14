#!/bin/bash

find /sbin/ /bin /usr/bin -type f -print0 |xargs -0 file |grep ELF |cut -d: -f1 |\
    while read file; do
	echo $file
	./readelf $file >/dev/null || break
	#readelf -a $file >/dev/null || break
    done
