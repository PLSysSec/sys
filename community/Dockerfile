
# Debian 10 requires LLVM 9 to be installed from source (no package, no backport),
# hence we use Debian 11 (testing) for now (to be released in 2021).
FROM debian:bullseye
MAINTAINER Ralf-Philipp Weinmann <ralf@comsecuris.com>

RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y apt-utils build-essential sudo screen tmux

# Add user
RUN useradd -c 'User' -G sudo -s /bin/bash -m -g users user

# Don't require password for sudo
RUN perl -i -pe 's/(%sudo.*) ALL/\1 NOPASSWD: ALL/' /etc/sudoers

# Install packages required to build boolector and sys
RUN apt-get update
RUN apt-get install -y llvm-9 haskell-stack git cmake curl

USER user
WORKDIR /home/user
RUN mkdir src
WORKDIR src
RUN git clone https://github.com/Boolector/btor2tools.git
RUN git clone https://github.com/Boolector/boolector.git
RUN git clone https://github.com/PLSysSec/sys.git
RUN git clone https://github.com/llvm-hs/llvm-hs

WORKDIR btor2tools
RUN ./configure.sh && (cd build; make)
RUN sudo cp build/lib/libbtor2parser.so  /usr/lib/ && \
    sudo mkdir /usr/include/btor2parser && \
    sudo cp src/btor2parser/btor2parser.h /usr/include/btor2parser/
WORKDIR ../boolector
RUN ./contrib/setup-lingeling.sh && ./configure.sh --shared --prefix /usr
RUN cd build && make && sudo make install

WORKDIR ../sys
RUN stack build

RUN sudo apt-get update
RUN sudo apt-get install -y clang-9
RUN sudo apt-get install -y libsfml-dev
RUN sudo ln /usr/bin/clang-9 /usr/bin/clang
RUN sudo ln /usr/bin/clang++-9 /usr/bin/clang++
RUN sudo ln /usr/bin/llvm-link-9 /usr/bin/llvm-link

RUN sudo apt-get install -y pip vim file
RUN sudo pip install wllvm
ENV PATH=$PATH:/home/user/.local/bin
ENV LLVM_COMPILER=clang
