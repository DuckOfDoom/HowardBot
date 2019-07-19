JAR_PATH='target/scala-2.12/HowardBot-assembly-0.1.jar'
CONFIG_PATH='config.json'

scp $JAR_PATH root@142.93.170.84:~/bot.jar
scp $CONFIG_PATH root@142.93.170.84:~/$CONFIG_PATH
