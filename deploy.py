#!python

import os
import sys
import subprocess
import argparse
import time
import paramiko

#from utils import notify
from scp import SCPClient
from datetime import datetime

date_time = datetime.now().strftime('%Y%m%d_%H%M%S')
build_dir = ''

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-prod', action='store_true')
    return vars(parser.parse_args())

def connect():
    key = paramiko.RSAKey.from_private_key_file(f"{os.environ['HOME']}/.ssh/id_rsa")
    ssh = paramiko.SSHClient()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    ssh.connect(hostname='142.93.170.84', username='root', pkey=key)
    return ssh

def scp_file(ssh, file_path, target_path):
    def progress4(filename, size, sent, peername):
        sys.stdout.write('(%s:%s) %s: (%s kBytes) %.2f%% \r' % (peername[0], peername[1], filename, int(size/1024), float(sent)/float(size)*100))

    with SCPClient(ssh.get_transport(), progress4=progress4) as scp:
        scp.put(file_path, target_path)

def build():
    print('-> Starting build with sbt assembly...')

    subprocess.run("sbt assembly", shell=True)#, stdout=subprocess.DEVNULL)
    return 'target/scala-2.12/HowardBot-assembly-0.1.jar'

def deploy(ssh, jar_path):
    git_revision = subprocess.check_output('git rev-parse HEAD').decode('utf-8')[0:6]
    build_name = f'howardbot_{git_revision}_{date_time}.jar'
    target_path=f'~/{build_dir}/{build_name}'
    print(f'-> Uploading "{jar_path}" to "{target_path}"')
    scp_file(ssh, jar_path, target_path)
    print(f'')
    print(f'-> Upload finished!')
    return build_name

def run(ssh, build_name):
    def cmd(c):
        print("$ " + c)
        stdin, stdout, stderr = ssh.exec_command(c)
        errors = stderr.read().decode('utf-8')
        if errors:
            print(errors.strip('\n'))
            raise Exception()

        output = (stdout.read()).decode('utf-8').strip('\n')
        if output: print(output)
        return output;

    def mkdir(f):
        return build_dir + "/" + f

    print("-> Executing run() via SSH...")

    backups_dir = mkdir('backups')
    last_build_name = cmd(f'cd {build_dir}; ls *.jar -1tr 2>/dev/null | head -n 1')
    if build_name in last_build_name: # do not back up new build lol
        last_build_name = None

    last_log_name = cmd(f'cd {build_dir}; ls *.log -1tr 2>/dev/null | head -n 1')

    pid_file = 'bot.pid'
    bot_pid = cmd(f'cat {mkdir(pid_file)}')

    cmd(f'mkdir -pv {backups_dir}/menu')
    cmd(f'mkdir -pv {backups_dir}/builds')
    cmd(f'mkdir -pv {backups_dir}/logs')

    if (bot_pid):
        print(f'-> Kiling previous instance with pid {bot_pid}')
        cmd(f'pkill {bot_pid}')

    if last_log_name:
        print(f'-> Backing up {mkdir(last_log_name)} to {backups_dir}/logs')
        cmd(f'mv -v {mkdir(last_log_name)} {backups_dir}/logs')
    else:
        print(f'-> No logs found, skipping backup.')

    if last_build_name:
        print(f'-> Backing up {mkdir(last_build_name)} to {backups_dir}/builds')
        cmd(f'mv -v {mkdir(last_build_name)} {backups_dir}/builds')
    else:
        print(f'-> No build found, skipping backup.')

    menu_backup_name = f"menu_{date_time}.json"
    print(f'-> Backing up menu.json to {backups_dir}/menu/{menu_backup_name}...')
    cmd(f'cp -v {mkdir("menu.json")} {backups_dir}/menu/{menu_backup_name}')

    print(f'-> Running new instance {mkdir(build_name)}...')
    new_log_file = build_name.replace('.jar', '.log')
    cmd(f'cd {build_dir}; nohup java -jar {build_name} > {new_log_file} 2>&1 & echo $! > {pid_file};')

    time.sleep(5)

    cmd('tail -f {mkdir(new_log_file)}')

if __name__ == '__main__':
    try:
        args = get_args()
        build_dir = 'prod' if args['prod'] else 'staging'
        artifact_name = build()

        with connect() as ssh:
            build_name = deploy(ssh, artifact_name)
            run(ssh, build_name)

    except Exception as ex:
        print('Exception occured: ' + str(ex))
    finally:
        print('done')
