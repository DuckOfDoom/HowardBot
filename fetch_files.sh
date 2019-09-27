#!/bin/bash

set -e

ADDRESS='root@142.93.170.84'
scp $ADDRESS:~/bot.log bot.log
scp $ADDRESS:~/menu.json menu.json
scp $ADDRESS:~/menu_changelog.txt menu_changelog.txt
