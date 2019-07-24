JAR_PATH='target/scala-2.12/HowardBot-assembly-0.1.jar'
CONFIG_PATH='config.json'
ADDRESS='root@142.93.170.84'

echo Starting sbt assembly...
sbt assembly

echo Assembly finished, pushing artifacts...

scp $JAR_PATH $ADDRESS:~/bot.jar
#scp $CONFIG_PATH $ADDRESS:~/$CONFIG_PATH
ssh $ADDRESS
