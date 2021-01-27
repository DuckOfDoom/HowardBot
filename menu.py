#!python

import json
import os
import pyperclip


with open(os.environ['HOME'] + "/Downloads/menu.json", encoding = 'utf-8', mode = 'r') as f:
    j = json.loads(f.read());
    c = 0
    for beer in j:
        c += 1
        name = beer['name']
        brewery = beer['breweryInfo']['name']
        if 'ogether' in name:
            print(name + " - " + brewery )

    print(c)
