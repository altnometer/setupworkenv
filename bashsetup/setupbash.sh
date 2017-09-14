#!/bin/bash
set -e

# cd to the folder before running
#https://github.com/altercation/solarized
if [ -z ${SUDO_USER} ]; then
    echo -e "\n\x1b[31;01m No \$SUDO_USER available, quiting ... \x1b[39;49;00m\n"
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -z "$DESKTOP_SESSION" -a "$DESKTOP_SESSION" == "lightdm-xsession" ];
then
    echo -e "$COL_GREEN seting up color scheme for xfce4 $COL_RESET"
    file_conf_orig="${SCRIPT_DIR}/terminalrc"
    dir_conf_dest="${HOME}/.config/Terminal"
    file_conf_dest="${dir_conf_dest}/terminalrc"
    sudo -u ${SUDO_USER} mkdir -pv "${dir_conf_dest}"
    sudo -u ${SUDO_USER} cp "$file_conf_orig"  "$file_conf_dest"
fi
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/bashrc ${HOME}/.bashrc
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/bash_profile ${HOME}/.bash_profile
