#!/bin/bash

# This scrip should set up emacs.

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

if hash emacs 2>/dev/null && [ -d "${EMACS_DEST_DIR}" ]; then
    echo -e "\n\x1b[33;01m emacs is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing, configuring emacs ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y emacs
    # documentation is in non-free repository, inlcude these repositories to install docs.
    apt-get install -y emacs-common-non-dfsg
fi

# link conf files ---------------------------------------------------------{{{

CONF_SOURCE_DIR="${SCRIPT_DIR}/conf"
CONF_DEST_DIR="${EMACS_DEST_DIR}/conf"

LISP_SOURCE_DIR="${SCRIPT_DIR}/lisp"
LISP_DEST_DIR="${EMACS_DEST_DIR}/lisp"

INIT_SOURCE_PATH="${SCRIPT_DIR}/init.el"
INIT_DEST_PATH="${EMACS_DEST_DIR}/init.el"

if [ -f $INIT_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $INIT_SOURCE_PATH to $INIT_SOURCE_PATH ... \x1b[39;49;00m\n"
    if [ -f "$INIT_SOURCE_PATH" ]; then
        rm $INIT_SOURCE_PATH
    fi
    if [ -h "$INIT_SOURCE_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $INIT_SOURCE_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $INIT_SOURCE_PATH $INIT_DEST_PATH
else
    echo -e "\n\x1b[31;01m $INIT_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

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

SCRIPT_DIR=$SCRIPT_DIR_OLD
# }}}
