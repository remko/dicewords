FROM debian:stretch

RUN apt-get update && apt-get install -y g++ make libboost-all-dev libsqlite3-dev

ADD . /opt/src
RUN cd /opt/src && make CXX=g++ all
