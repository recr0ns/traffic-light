FROM ubuntu
ADD ./rel ./trafficLight
WORKDIR ./trafficLight

RUN chmod +x ./build.sh
