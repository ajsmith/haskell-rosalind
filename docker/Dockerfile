FROM fedora:34
MAINTAINER Alexander Smith <ajsmith@acm.org>

WORKDIR /root
COPY docker/setup.sh .
COPY docker/packages.txt .
RUN /bin/bash setup.sh
RUN useradd -u 1001 -m me

USER me
WORKDIR /home/me
