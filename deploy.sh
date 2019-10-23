#!/bin/bash

set -e

JAR_PATH='target/scala-2.12/HowardBot-assembly-0.1.jar'
CONFIG_PATH='config.json'
ADDRESS='root@142.93.170.84'

echo Starting sbt assembly...
sbt assembly

echo Assembly finished, pushing artifacts...

echo Pushing $JAR_PATH ...
scp $JAR_PATH $ADDRESS:~/bot_new.jar

if [ "$1" == "-conf" ]; then
  echo Pushing $CONFIG_PATH ...
  scp $CONFIG_PATH $ADDRESS:~/$CONFIG_PATH
fi

ssh $ADDRESS << EOF
  pkill "java"
  mv bot.log logs/$(date +%Y_%m_%d_%H%M).log
  mv bot.jar builds/bot_$(date +%Y_%m_%d_%H%M).jar
  cp menu.json menu_backups/menu_$(date +%Y_%m_%d_%H%M).json
  mv bot_new.jar bot.jar
  nohup java -jar bot.jar > bot.log 2>&1 & 
  sleep 3
  tail -f bot.log
EOF

#ssh $ADDRESS
