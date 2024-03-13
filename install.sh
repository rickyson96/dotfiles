#!/bin/bash
export DEBIAN_FRONTEND=noninteractive

sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa

sudo apt update
sudo apt upgrade -y
sudo apt install -y fish ripgrep stow

# install emacs build tools
sudo apt install -y build-essential gcc-10 libgccjit-10-dev texinfo

# install tree-sitter
git clone https://github.com/tree-sitter/tree-sitter.git
pushd tree-sitter || exit
make
sudo make install
popd

# install emacs
git clone https://git.savannah.gnu.org/git/emacs.git
pushd emacs || exit
export CC=/usr/bin/gcc-10 \
       CXX=/usr/bin/gcc-10 \
       LD_LIBRARY_PATH=/usr/local/lib
       
./configure \
    --with-tree-sitter \
    --with-native-compilation \
    --without-compress-install \
    --with-x-toolkit=no \
    --with-jpeg=ifavailable \
    --with-xpm=ifavailable \
    --with-tiff=ifavailable \
    --with-png=ifavailable \
    --with-gif=ifavailable \
    CC=gcc-10

make bootstrap
make


stow -t "$HOME" . 
