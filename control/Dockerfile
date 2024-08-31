FROM debian:trixie

RUN apt-get update && \
    apt-get install -y g++-14 git cmake build-essential \
    emacs fonts-inconsolata clangd ninja-build libgtest-dev \
    libsdl2-dev

ENV DISPLAY :0
