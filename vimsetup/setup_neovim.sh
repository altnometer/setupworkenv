#!/bin/bash
set -e

# vars --------------------------------------------------------------------{{{
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# }}}

# This scrip should set up neovim
# check if run with sudo -E -----------------------------------------------{{{
if [[ $EUID -ne 0 ]]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    exit 1
fi
if [ $HOME  = '/root' ]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    exit 1
fi
if [ -z ${SUDO_USER} ]; then
    echo -e "\n\x1b[31;01m No \$SUDO_USER available, quiting ... \x1b[39;49;00m\n"
    exit 1
fi
# }}}

# run apt-get update ------------------------------------------------------{{{
# if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
#     echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
#     apt-get update
# fi
# }}}

# setup neovim ------------------------------------------------------------{{{
# install packages
# apt-get install -y vim-nox curl git wgepython-pip exuberant-ctags
# NVIM_INST_DIR="${HOME}/.local/share/nvim"
NVIM_INST_DIR="${HOME}/Repos/neovim"
# NVIM_EXEC_FILE="${NVIM_INST_DIR}/nvim.appimage"
NVIM_EXEC_FILE_SOURCE="${NVIM_INST_DIR}/nvim"
NVIM_EXEC_FILE_DEST="/usr/local/bin/nvim"
if hash nvim 2>/dev/null; then
    echo -e "\n\x1b[33;01m neovim is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing neovim ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y curl python-neovim python3-neovim silversearcher-ag ripgrep
    # apt-get install -y fuse
    apt-get install -y exuberant-ctags
    apt-get install -y ninja-build gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip
    # sudo -u ${SUDO_USER} mkdir -p $NVIM_INST_DIR
    # sudo -u ${SUDO_USER} curl -Lo $NVIM_EXEC_FILE_SOURCE https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
    if [ ! -d "$NVIM_INST_DIR" ] ; then
        sudo -u ${SUDO_USER} git clone https://github.com/neovim/neovim $NVIM_INST_DIR
    fi
    cd $NVIM_INST_DIR
    sudo -u ${SUDO_USER} git checkout v0.3.1
    # sudo -u ${SUDO_USER} make
    make install
    # chmod u+x $NVIM_EXEC_FILE_SOURCE
    # rm -f $NVIM_EXEC_FILE_DEST
    # ln -s $NVIM_EXEC_FILE_SOURCE $NVIM_EXEC_FILE_DEST
fi

if hash pip 2>/dev/null; then
    echo -e "\n\x1b[33;01m pip is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m installing pip ...\x1b[39;49;00m\n" && sleep 1
    apt-get install -y python-pip
fi

if hash pip3 2>/dev/null; then
    echo -e "\n\x1b[33;01m pip3 is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m installing pip3 ...\x1b[39;49;00m\n" && sleep 1
    apt-get install -y python3-pip
fi

if pip show pynvim 1>/dev/null; then
    echo -e "\n\x1b[33;01m pynvim is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m installing pynvim ...\x1b[39;49;00m\n" && sleep 1
    pip install pynvim
fi

if pip3 show pynvim 1>/dev/null; then
    echo -e "\n\x1b[33;01m pynvim (python3) is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m installing pynvim (python3) ...\x1b[39;49;00m\n" && sleep 1
    pip3 install pynvim
fi
# }}}

# setup junegunn/vim-plug -------------------------------------------------{{{
if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim  ];
then
    # Setup vim-plug to manage your plugins
    sudo -u ${SUDO_USER} curl -fLo ${HOME}/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
# }}}

# setup JakeBecker/elixir-ls
if hash mix 2>/dev/null; then
    ELIXIR_LS_DEST_DIR="${HOME}/.local/share/elixir_ls"
    rm -r ${ELIXIR_LS_DEST_DIR}
    sudo -u ${SUDO_USER} git clone https://github.com/JakeBecker/elixir-ls $ELIXIR_LS_DEST_DIR
    cd ${ELIXIR_LS_DEST_DIR} && mkdir rel
    sudo -u ${SUDO_USER} mix local.hex local.rebar --force
    sudo -u ${SUDO_USER} mix local.rebar --force
    sudo -u ${SUDO_USER} mix deps.get
    sudo -u ${SUDO_USER} mix mix compile
    sudo -u ${SUDO_USER} mix elixir_ls.release -o rel
else
    echo -e "\n\x1b[31;01m Elixir is not installed, not installing JakeBecker/elixir-ls. \x1b[39;49;00m\n" && sleep 1
fi
# }}}

# linking config ----------------------------------------------------------{{{
VIMRC_PATH="${SCRIPT_DIR}/init.vim"
VIMRC_DEST_DIR="${HOME}/.config/nvim"
VIMRC_DEST="${VIMRC_DEST_DIR}/init.vim"
echo -e "\n\x1b[33;01m Creating $VIMRC_DEST_DIR dir ... \x1b[39;49;00m\n"
sudo -u ${SUDO_USER} mkdir -p $VIMRC_DEST_DIR
if [ -f $VIMRC_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $VIMRC_PATH to $VIMRC_DEST ... \x1b[39;49;00m\n"
    if [ -f "$VIMRC_DEST" ]; then
        rm $VIMRC_DEST
    fi
    if [ -h "$VIMRC_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $VIMRC_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $VIMRC_PATH $VIMRC_DEST
else
    echo -e "\n\x1b[31;01m $VIMRC_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# linking custom plugins --------------------------------------------------{{{
SOURCE_DIR="${SCRIPT_DIR}/myvimplugins"
DEST_DIR="${HOME}/.local/share/nvim/myvimplugins"
if [ -d $SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $SOURCE_DIR to $DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$DEST_DIR" ]; then
        rm $DEST_DIR
    fi
	sudo -u ${SUDO_USER} ln -s $SOURCE_DIR $DEST_DIR
else
echo -e "\n\x1b[31;01m $SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# linking custom neosnippets ----------------------------------------------{{{
SOURCE_DIR="${SCRIPT_DIR}/snippets"
DEST_DIR="${HOME}/.local/share/nvim/snippets"
if [ -d $SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $SOURCE_DIR to $DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$DEST_DIR" ]; then
        rm $DEST_DIR
    fi
	sudo -u ${SUDO_USER} ln -s $SOURCE_DIR $DEST_DIR
else
echo -e "\n\x1b[31;01m $SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}
