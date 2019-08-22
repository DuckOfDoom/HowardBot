#!/bin/bash

set -e

ADDRESS='root@142.93.170.84'
scp $ADDRESS:~/log.txt log.txt
