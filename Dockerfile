FROM python:3.9

WORKDIR /data

RUN apt-get update && apt-get install -y build-essential pkg-config git ninja-build meson

# First install serd-1
RUN git clone --branch 1.x https://github.com/drobilla/serd.git
RUN cd serd && meson setup build && cd build && meson compile && meson install && cd /data
COPY . .
RUN pip install Cython
RUN CYTHONIZE=1 pip install .
# If you don't run ldconfig, the lib is not found, see https://github.com/drobilla/serd/issues/15
