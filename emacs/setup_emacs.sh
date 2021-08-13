#!/bin/bash

# This scrip should set up emacs.

# check if script is run with 'sudo -E'
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
SCRIPT_DIR_OLD=$SCRIPT_DIR
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
EMACS_DEST_DIR="${HOME}/.local/share/emacs/my.emacs.d"

# consider installing the latest version, follow instructions in the link
# https://www.emacswiki.org/emacs/EmacsSnapshotAndDebian

# if hash emacs 2>/dev/null && [ -d "${EMACS_DEST_DIR}" ]; then
if hash emacs 2>/dev/null; then
    echo -e "\n\x1b[33;01m emacs is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing supporting packages ...  \x1b[39;49;00m\n" && sleep 1
    # !!! sound: install pulseaudio, alsa-* packages.
    apt-get install -y xorg xinput firefox-esr \
        feh mupdf zathura
    apt-get install -y silversearcher-ag ripgrep pass
    apt-get install -y hddtemp lm-sensors upower ispell dictionaries-common iamerican
    echo -e "\n\x1b[33;01m add repositories for emacs docs ...  \x1b[39;49;00m\n" && sleep 1
    # documentation is in non-free repository, inlcude these repositories to install docs.
    # apt-get install -y emacs-common-non-dfsg

    # # tools for compiling
    # apt-get install -y autoconf make gcc texinfo xorg-dev libgtk-3-dev libgccjit-10-dev cmake libtool-bin\
    #         libjpeg-dev libgif-dev libpng-dev libtiff-dev libgnutls28-dev libtinfo-dev

    # tools for compiling
    apt-get install -y autoconf make gcc texinfo libgccjit-10-dev cmake libtool-bin \
            libjpeg-dev libgif-dev libpng-dev libtiff-dev libgnutls28-dev libtinfo-dev

    echo -e "\n\x1b[33;01m Installing, configuring emacs ...  \x1b[39;49;00m\n" && sleep 1

    cd /tmp
    git clone git://git.savannah.gnu.org/emacs.git
    cd emacs
    ./autogen.sh
    ./configure --with-native-compilation --with-sound=no --with-cairo --with-x=yes --with-x-toolkit=no \
          --with-mailutils --without-toolkit-scroll-bars
    make -j$(nproc)
    make install
    #make clean

    #apt-get install -y emacs
fi

# link files

INIT_SOURCE_PATH="${SCRIPT_DIR}/init.el"
INIT_DEST_PATH="${EMACS_DEST_DIR}/init.el"

if [ -f $INIT_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $INIT_SOURCE_PATH to $INIT_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$INIT_DEST_PATH" ]; then
        rm $INIT_DEST_PATH
    fi
    if [ -h "$INIT_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $INIT_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $INIT_SOURCE_PATH $INIT_DEST_PATH
else
    echo -e "\n\x1b[31;01m $INIT_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi


CONF_SOURCE_DIR="${SCRIPT_DIR}/config"
CONF_DEST_DIR="${EMACS_DEST_DIR}/config"

if [ -d $CONF_SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $CONF_SOURCE_DIR to $CONF_DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$CONF_DEST_DIR" ]; then
        rm -r $CONF_DEST_DIR
    fi
    if [ -h "$CONF_DEST_DIR" ]; then  # -h, true if file exist and a symbolic link.
        rm -r $CONF_DEST_DIR
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $CONF_SOURCE_DIR $CONF_DEST_DIR
else
    echo -e "\n\x1b[31;01m $CONF_SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

LISP_SOURCE_DIR="${SCRIPT_DIR}/lisp"
LISP_DEST_DIR="${EMACS_DEST_DIR}/lisp"

if [ -d $LISP_SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $LISP_SOURCE_DIR to $LISP_DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$LISP_DEST_DIR" ]; then
        rm -r $LISP_DEST_DIR
    fi
    if [ -h "$LISP_DEST_DIR" ]; then  # -h, true if file exist and a symbolic link.
        rm -r $LISP_DEST_DIR
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $LISP_SOURCE_DIR $LISP_DEST_DIR
else
    echo -e "\n\x1b[31;01m $LISP_SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

GITIGNORE_SOURCE_PATH="${SCRIPT_DIR}/../.gitignore"
GITIGNORE_DEST_PATH="${EMACS_DEST_DIR}/.gitignore"

if [ -f $GITIGNORE_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $GITIGNORE_SOURCE_PATH to $GITIGNORE_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$GITIGNORE_DEST_PATH" ]; then
        rm $GITIGNORE_DEST_PATH
    fi
    if [ -h "$GITIGNORE_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $GITIGNORE_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $GITIGNORE_SOURCE_PATH $GITIGNORE_DEST_PATH
else
    echo -e "\n\x1b[31;01m $GITIGNORE_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

ESHELL_DIR_SOURCE_PATH="${BACKUP_DIR}/emacs/eshell"
ESHELL_DIR_DEST_PATH="${EMACS_DEST_DIR}/eshell"

if [ -d $ESHELL_DIR_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $ESHELL_DIR_SOURCE_PATH to $ESHELL_DIR_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -d "$ESHELL_DIR_DEST_PATH" ]; then
        rm -r $ESHELL_DIR_DEST_PATH
    fi
    if [ -h "$ESHELL_DIR_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $ESHELL_DIR_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $ESHELL_DIR_SOURCE_PATH $ESHELL_DIR_DEST_PATH
else
    echo -e "\n\x1b[31;01m $ESHELL_DIR_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

HISTORY_SOURCE_PATH="${BACKUP_DIR}/emacs/history"
HISTORY_DEST_PATH="${EMACS_DEST_DIR}/history"

if [ -f $HISTORY_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $HISTORY_SOURCE_PATH to $HISTORY_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$HISTORY_DEST_PATH" ]; then
        rm $HISTORY_DEST_PATH
    fi
    if [ -h "$HISTORY_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $HISTORY_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $HISTORY_SOURCE_PATH $HISTORY_DEST_PATH
else
    echo -e "\n\x1b[31;01m $HISTORY_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

SAVEHISTORY_SOURCE_PATH="${BACKUP_DIR}/emacs/savehist"
SAVEHISTORY_DEST_PATH="${EMACS_DEST_DIR}/savehist"

if [ -f $SAVEHISTORY_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $SAVEHISTORY_SOURCE_PATH to $SAVEHISTORY_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$SAVEHISTORY_DEST_PATH" ]; then
        rm $SAVEHISTORY_DEST_PATH
    fi
    if [ -h "$SAVEHISTORY_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $SAVEHISTORY_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $SAVEHISTORY_SOURCE_PATH $SAVEHISTORY_DEST_PATH
else
    echo -e "\n\x1b[31;01m $SAVEHISTORY_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

RECENTF_SOURCE_PATH="${BACKUP_DIR}/emacs/recentf"
RECENTF_DEST_PATH="${EMACS_DEST_DIR}/recentf"

if [ -f $RECENTF_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $RECENTF_SOURCE_PATH to $RECENTF_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$RECENTF_DEST_PATH" ]; then
        rm $RECENTF_DEST_PATH
    fi
    if [ -h "$RECENTF_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $RECENTF_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $RECENTF_SOURCE_PATH $RECENTF_DEST_PATH
else
    echo -e "\n\x1b[31;01m $RECENTF_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

# list of multicursor commands allowed for multiplo cursors
MC_LISTS_SOURCE_PATH="${BACKUP_DIR}/emacs/.mc-lists.el"
MC_LISTS_DEST_PATH="${EMACS_DEST_DIR}/.mc-lists.el"

if [ -f $MC_LISTS_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $MC_LISTS_SOURCE_PATH to $MC_LISTS_DEST_PATH ... \x1b[39;49;00m\n"
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -fs $MC_LISTS_SOURCE_PATH $MC_LISTS_DEST_PATH
else
    echo -e "\n\x1b[31;01m $MC_LISTS_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

# if [ -f $MC_LISTS_SOURCE_PATH ];
# then
#     echo -e "\n\x1b[33;01m Linking $MC_LISTS_SOURCE_PATH to $MC_LISTS_DEST_PATH ... \x1b[39;49;00m\n"
#     if [ -f "$MC_LISTS_DEST_PATH" ]; then
#         rm $MC_LISTS_DEST_PATH
#     fi
#     if [ -h "$MC_LISTS_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
#         rm $MC_LISTS_DEST_PATH
#     fi
#     sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
#     sudo -u ${SUDO_USER} ln -s $MC_LISTS_SOURCE_PATH $MC_LISTS_DEST_PATH
# else
#     echo -e "\n\x1b[31;01m $MC_LISTS_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
# 	exit 1
# fi


# ** link EMACS_CONF_DIR (${HOME}/.emacs.d) to EMACS_DEST_DIR
EMACS_CONF_DIR="${HOME}/.emacs.d"

if [ -d $EMACS_DEST_DIR ];
then
    echo -e "\n\x1b[33;01m Removing default $EMACS_CONF_DIR ... \x1b[39;49;00m\n"
    if [ -d "$EMACS_CONF_DIR" ]; then
        rm -r $EMACS_CONF_DIR
    fi
    if [ -h "$EMACS_CONF_DIR" ]; then  # -h, true if file exist and a symbolic link.
        rm $EMACS_CONF_DIR
    fi
    echo -e "\n\x1b[33;01m Linking $EMACS_DEST_DIR to $EMACS_CONF_DIR ... \x1b[39;49;00m\n"
    sudo -u ${SUDO_USER} ln -s $EMACS_DEST_DIR $EMACS_CONF_DIR
else
    echo -e "\n\x1b[31;01m $EMACS_DEST_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

SCRIPT_DIR=$SCRIPT_DIR_OLD
