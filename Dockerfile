FROM ubuntu:18.04
RUN apt-get -y update && \
  apt-get -y upgrade && \
  apt-get -y install libgmp10 libssl1.1
WORKDIR /root/
COPY out .
CMD ["./out"]