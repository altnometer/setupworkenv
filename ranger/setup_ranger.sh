#!/bin/bash

# This scrip should set up ranger.

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
CONFIG_DEST_DIR="${HOME}/.config/ranger"

if hash ranger 2>/dev/null && [ -d "${CONFIG_DEST_DIR}" ]; then
    echo -e "\n\x1b[33;01m ranger is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing, configuring ranger ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y ranger
fi

# link rc.conf ------------------------------------------------------------{{{
RC_CONF_SOURCE_PATH="${SCRIPT_DIR}/rc.conf"
RC_CONF_DEST_PATH="${CONFIG_DEST_DIR}/rc.conf"
if [ -f $RC_CONF_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $RC_CONF_SOURCE_PATH to $RC_CONF_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$RC_CONF_DEST_PATH" ]; then
        rm $RC_CONF_DEST_PATH
    fi
    if [ -h "$RC_CONF_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $RC_CONF_DEST_PATH
    fi
	sudo -u ${SUDO_USER} ln -s $RC_CONF_SOURCE_PATH $RC_CONF_DEST_PATH
else
    echo -e "\n\x1b[31;01m $RC_CONF_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link commands.py --------------------------------------------------------{{{
COMMANDS_SOURCE_PATH="${SCRIPT_DIR}/commands.py"
COMMANDS_DEST_PATH="${CONFIG_DEST_DIR}/commands.py"
if [ -f $COMMANDS_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $COMMANDS_SOURCE_PATH to $COMMANDS_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$COMMANDS_DEST_PATH" ]; then
        rm $COMMANDS_DEST_PATH
    fi
    if [ -h "$COMMANDS_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $COMMANDS_DEST_PATH
    fi
	sudo -u ${SUDO_USER} ln -s $COMMANDS_SOURCE_PATH $COMMANDS_DEST_PATH
else
    echo -e "\n\x1b[31;01m $COMMANDS_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}
