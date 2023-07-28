FROM ubuntu:23.04

WORKDIR /data

RUN apt-get update && apt-get install -y python3-pip python3-virtualenv python3-dev build-essential pkg-config git ninja-build meson python3-sphinx
RUN virtualenv /data/
ENV PATH="/data/bin:$PATH"
RUN /data/bin/pip install Cython 

COPY . .

RUN meson setup builddir && ninja -C builddir 
