#!/bin/bash
nohup java -jar $(sudo sh -c 'ls -1tr *.jar | head -n 1') > bot.log 2>&1 & echo $! > bot.pid
sleep 5
tail -f bot.log
