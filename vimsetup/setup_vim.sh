#!/bin/bash

# This scrip should set up vim with options in .vimrc
# .vimrc file should be copied to ~/ folder.
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
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VIMRC_PATH="${SCRIPT_DIR}/vimrc"
if [ -f $VIMRC_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $VIMRC_PATH to $HOME/.vimrc ... \x1b[39;49;00m\n"
    if [ -f "$HOME/.vimrc" ]; then
        rm $HOME/.vimrc
    fi
    if [ -h "$HOME/.vimrc" ]; then  # -h, true if file exist and a symbolic link.
        rm $HOME/.vimrc
    fi
	sudo -u ${SUDO_USER} ln -s $VIMRC_PATH $HOME/.vimrc
else
    echo -e "\n\x1b[31;01m vimrc does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
    echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
    apt-get update
fi
apt-get install -y vim-nox curl git wget python-pip exuberant-ctags
#pip install pep8
pip install pyflakes
pip install flake8
#pip install pylint
# # Setup Pathogen to manage your plugins
# PATHOGEN_PATH=$HOME/redmoo/auct/setup/vimsetup/pathogen.vim
# sudo -u ${SUDO_USER} mkdir -pv ~/.vim/autoload ~/.vim/bundle
# #sudo -u ${SUDO_USER} curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
# sudo -u ${SUDO_USER} cp -f ${PATHOGEN_PATH} ~/.vim/autoload/pathogen.vim
# # Now you can install any plugin into a .vim/bundle/plugin-name/ folder

# Setup vim-plug to manage your plugins
sudo -u ${SUDO_USER} curl -fLo ${HOME}/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# if [ -d "$HOME/.vim/bundle/vim-airline" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-airline directory exists, not cloning.  \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-airline ... \x1b[39;49;00m\n" && sleep 1
#     sudo -u ${SUDO_USER} git clone https://github.com/vim-airline/vim-airline ~/.vim/bundle/vim-airline
# fi

# # Settings for ctrlp
# if [ -d "$HOME/.vim/bundle/ctrlp.vim" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/ctrlp.vim directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning ctrlp.vim ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/kien/ctrlp.vim.git
# fi

# Setting up syntastic code checking
# if [ -d "$HOME/.vim/bundle/syntastic" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/syntastic directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning syntastic ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/scrooloose/syntastic.git
# fi

# # Setting up vim-javascript bundle
# if [ -d "$HOME/.vim/bundle/vim-javascript" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-javascript directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-javascript ... \x1b[39;49;00m\n" && sleep 1
#     sudo -u ${SUDO_USER} git clone https://github.com/pangloss/vim-javascript.git ~/.vim/bundle/vim-javascript
# fi

# # Setting up React JSX syntax highlighting and indenting
# if [ -d "$HOME/.vim/bundle/vim-jsx" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-jsx directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-jsx ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/mxw/vim-jsx.git
# fi

# Create a ropeproject by executing ``:RopeOpenProject``."""
# Add your venv to ``.ropeproject/config.py``!'
# Settings for python-mode
# python-mode complaints
# when standard library methods shown, vim slows down
# sometimes code cheking stops responding
#git clone https://github.com/klen/python-mode
# remove python-mode
# if [ -d "$HOME/.vim/bundle/python-mode" ]; then
#     rm -rfv ~/.vim/bundle/python-mode
# fi
# install some of the python-mode features separately
# Code checking
# Strong code completion (rope)
# Go to definition (<C-c>g for :RopeGotoDefinition)

if [ -f "$HOME/.vim/ftplugin/python_editing.vim" ]; then
    rm "$HOME/.vim/ftplugin/python_editing.vim"
    # echo -e "\n\x1b[33;01m $HOME/.vim/ftplugin/python_editing.vim directory exists, not fetching. \x1b[39;49;00m\n" && sleep 1
else
    true
    # echo -e "\n\x1b[33;01m fetching python_editing.vim ... \x1b[39;49;00m\n" && sleep 1
    # sudo -u ${SUDO_USER} mkdir -p ~/.vim/ftplugin
    # sudo -u ${SUDO_USER} wget -O ~/.vim/ftplugin/python_editing.vim http://www.vim.org/scripts/download_script.php?src_id=5492
fi
# # Python folding
# if [ -d "$HOME/.vim/bundle/SimpylFold" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/SimpylFold directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning SimpylFold ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/tmhedberg/SimpylFold.git
# fi
# # Color scheme
# # colorscheme wombat256mod
# if [ -f "$HOME/.vim/colors/wombat256mod.vim" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/colors/wombat256mod.vim exists, not fetching. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m fetching wombat256mod.vim \x1b[39;49;00m\n" && sleep 1
#     sudo -u ${SUDO_USER} mkdir -p ~/.vim/colors && cd ~/.vim/colors
#     sudo -u ${SUDO_USER} wget -O wombat256mod.vim http://www.vim.org/scripts/download_script.php?src_id=13400
# fi

# colorscheme seoul256
#if [ -f "$HOME/.vim/colors/seoul256.vim" ]; then
#    echo -e "\n\x1b[33;01m $HOME/.vim/colors/seoul256.vim exists, not fetching. \x1b[39;49;00m\n" && sleep 1
#else
#    echo -e "\n\x1b[33;01m cloning seoul256.vim \x1b[39;49;00m\n" && sleep 1
#    sudo -u ${SUDO_USER} mkdir -p ~/.vim/bundle && cd ~/.vim/bundle
#    sudo -u ${SUDO_USER} git clone git://github.com/junegunn/seoul256.vim
#    sudo -u ${SUDO_USER} mkdir -p ~/.vim/colors
#    sudo -u ${SUDO_USER} cp ~/.vim/bundle/seoul256.vim/colors/seoul256.vim ~/.vim/colors
#fi
COLOR_SCHEME_SEOUL256_PATH="${SCRIPT_DIR}/seoul256.vim"
echo -e "\n\x1b[33;01m linking $HOME/.vim/colors/seoul256.vim to ${COLOR_SCHEME_SEOUL256_PATH}. \x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} mkdir -p ~/.vim/colors
sudo -u ${SUDO_USER} ln -sf ${COLOR_SCHEME_SEOUL256_PATH} $HOME/.vim/colors/seoul256.vim

# # colorscheme zenburn
# if [ -f "$HOME/.vim/colors/zenburn.vim" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/colors/zenburn.vim exists, not fetching. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning zenburn.vim \x1b[39;49;00m\n" && sleep 1
#     sudo -u ${SUDO_USER} mkdir -p ~/.vim/bundle && cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/jnurmine/Zenburn/
#     sudo -u ${SUDO_USER} mkdir -p ~/.vim/colors
#     sudo -u ${SUDO_USER} cp ~/.vim/bundle/Zenburn/colors/zenburn.vim ~/.vim/colors
# fi

# # Install fugitive (git wrapper)
# if [ -d "$HOME/.vim/bundle/vim-fugitive" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-fugitive directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-fugitive ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-fugitive.git
# fi

# # Install unimpaired.vim: pairs of navigation mappings from tpope
# if [ -d "$HOME/.vim/bundle/vim-unimpaired" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-unimpaired directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-unimpaired ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-unimpaired.git
# fi

# # Install vim-surround.vim
# if [ -d "$HOME/.vim/bundle/vim-surround" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-surround directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-surround ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-surround.git
# fi

# # Install vim-repeat.vim
# if [ -d "$HOME/.vim/bundle/vim-repeat" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-repeat directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-repeat ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-repeat.git
# fi

# # Install ReplaceWithRegister.vim
# if [ -d "$HOME/.vim/bundle/ReplaceWithRegister" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/ReplaceWithRegister directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning ReplaceWithRegister ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/vim-scripts/ReplaceWithRegister
# fi

# # Install vim-indent-object.vim
# if [ -d "$HOME/.vim/bundle/vim-indent-object" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-indent-object directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-indent-object ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/michaeljsmith/vim-indent-object
# fi

# # Install vim-commentary.vim
# if [ -d "$HOME/.vim/bundle/vim-commentary" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-commentary directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-commentary ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-commentary.git
# fi

# # Install braceless.vim
# if [ -d "$HOME/.vim/bundle/braceless.vim" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/braceless directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning braceless ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/tweekmonster/braceless.vim
# fi

# # Install vim-easymotion
# if [ -d "$HOME/.vim/bundle/vim-easymotion" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-easymotion directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-easymotion ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/easymotion/vim-easymotion
# fi

# # Install vim-indentwise
# if [ -d "$HOME/.vim/bundle/vim-indentwise" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-indentwise directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-indentwise ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/jeetsukumaran/vim-indentwise.git
# fi

# # Install ansible-vim
# if [ -d "$HOME/.vim/bundle/ansible-vim" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/ansible-vim directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning ansible-vim ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/pearofducks/ansible-vim
# fi

# # Install vim-mustache-handlebars
# if [ -d "$HOME/.vim/bundle/vim-mustache-handlebars" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-mustache-handlebars directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-mustache-handlebars ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/mustache/vim-mustache-handlebars.git
# fi

# Install vim-go # installed with vim-plug
# if [ -d "$HOME/.vim/bundle/vim-go" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-go directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-go ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone https://github.com/fatih/vim-go.git
# fi
# Install vim-dispatch
# Leverage the power of Vim's compiler plugins without being bound by
# synchronicity. Kick off builds and test suites using one of several
# asynchronous adapters (including tmux, screen, iTerm, Windows, and a
# headless mode), and when the job completes, errors will be loaded and
# parsed automatically.
# if [ -d "$HOME/.vim/bundle/vim-dispatch" ]; then
#     echo -e "\n\x1b[33;01m $HOME/.vim/bundle/vim-dispatch directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m cloning vim-dispatch ... \x1b[39;49;00m\n" && sleep 1
#     cd ~/.vim/bundle
#     sudo -u ${SUDO_USER} git clone git://github.com/tpope/vim-dispatch.git
# fi
