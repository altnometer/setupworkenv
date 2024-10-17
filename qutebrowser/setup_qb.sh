#!/bin/bash

# This scrip should set up qutebrowser.

# Check if the scrip is run with 'sudo -E'
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
CONFIG_DEST_DIR="${HOME}/.config/qutebrowser"
DOWNLOAD_DIR="${HOME}/Downloads"
BACKUP_DIR="${HOME}/backup"

if hash qutebrowser 2>/dev/null && [ -d "${CONFIG_DEST_DIR}" ]; then
    echo -e "\n\x1b[33;01m qutebrowser is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing, configuring qutebrowser ...  \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} mkdir -p ${DOWNLOAD_DIR}
    apt-get install -y qutebrowser
fi

#** link config.py
CONF_SOURCE_PATH="${SCRIPT_DIR}/config.py"
CONF_DEST_PATH="${CONFIG_DEST_DIR}/config.py"
if [ -f $CONF_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $CONF_SOURCE_PATH to $CONF_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$CONF_DEST_PATH" ]; then
        rm $CONF_DEST_PATH
    fi
    if [ -h "$CONF_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $CONF_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $CONFIG_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $CONF_SOURCE_PATH $CONF_DEST_PATH
else
    echo -e "\n\x1b[31;01m $CONF_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

#** link quickmarks

QUICKMARKS_SOURCE_PATH="${BACKUP_DIR}/qutebrowser/quickmarks"
QUICKMARKS_DEST_PATH="${HOME}/.config/qutebrowser/quickmarks"

if [ -f $QUICKMARKS_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $QUICKMARKS_SOURCE_PATH to $QUICKMARKS_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$QUICKMARKS_DEST_PATH" ]; then
        rm $QUICKMARKS_DEST_PATH
    fi
    if [ -h "$QUICKMARKS_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $QUICKMARKS_DEST_PATH
    fi
    sudo -u ${SUDO_USER} ln -s $QUICKMARKS_SOURCE_PATH $QUICKMARKS_DEST_PATH
else
    echo -e "\n\x1b[31;01m $QUICKMARKS_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

SCRIPT_DIR=$SCRIPT_DIR_OLD

#** link default session

SESSION_SOURCE_PATH="${BACKUP_DIR}/qutebrowser/default_session.yml"
SESSION_DEST_PATH="${HOME}/.local/share/qutebrowser/sessions/default.yml"

if [ -f $SESSION_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $SESSION_SOURCE_PATH to $SESSION_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$SESSION_DEST_PATH" ]; then
        rm $SESSION_DEST_PATH
    fi
    if [ -h "$SESSION_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $SESSION_DEST_PATH
    fi
    sudo -u ${SUDO_USER} ln -s $SESSION_SOURCE_PATH $SESSION_DEST_PATH
else
    echo -e "\n\x1b[31;01m $SESSION_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

SCRIPT_DIR=$SCRIPT_DIR_OLD
