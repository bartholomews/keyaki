#!/usr/bin/env python

import yaml

with open("server/package.yaml", 'r') as stream:
    try:
        data = yaml.safe_load(stream)
        print(data['version'])
    except yaml.YAMLError as exc:
        raise ValueError(exc)
