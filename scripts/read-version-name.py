#!/usr/bin/env python3

with open("keyaki.cabal", 'r') as stream:
    key = "version"
    while True:
        line = stream.readline()
        if not line:
            stream.close()
            raise Exception(f'Key not found: {key}')

        arr = line.split(':', 1)
        if arr[0] == key:
            print(arr[1].lstrip())
            break
