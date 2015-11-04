#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case $1 in
  start|stop|ping)
    echo ./trafficLight/bin/trafficLight $1
    exec $DIR/trafficLight/bin/trafficLight $1
    ;;
  *)
    echo "u can use {start|stop|ping}"
esac
