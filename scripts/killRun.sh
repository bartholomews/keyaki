#!/bin/bash
pkill keyaki
stack exec keyaki &
sleep 0.6
echo -e "server running with PID={$(pgrep keyaki)} (if that's empty, then the server is not running)"