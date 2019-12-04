#!/home/bart/.pyenv/shims/python3

import json
import sys

import yaml

json.dump(yaml.load(sys.stdin, Loader=yaml.SafeLoader), sys.stdout, indent=4)
