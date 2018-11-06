#!/bin/bash
set -e

# This scrip should set up tmux
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

# Install tmux ------------------------------------------------------------{{{
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TMUX_DEST_DIR="${HOME}/.tmux"
TMUX_PLUGIN_DIR="${TMUX_DEST_DIR}/plugins"
TMUX_PLUGIN_TPM_DIR="${TMUX_PLUGIN_DIR}/tpm"
TMUX_CONFIG_SOURCE=${SCRIPT_DIR}/tmux.conf
TMUX_CONFIG_DEST=${HOME}/.tmux.conf
if hash tmux 2>/dev/null; then
    echo -e "\n\x1b[33;01m tmux is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing, configuring tmux ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y tmux
    if [ ! -d "$TMUX_PLUGIN_TPM_DIR" ] ; then
        sudo -u ${SUDO_USER} git clone https://github.com/tmux-plugins/tpm ${TMUX_PLUGIN_TPM_DIR}
    fi
fi
echo -e "\n\x1b[33;01m Linking $TMUX_CONFIG_SOURCE to $TMUX_CONFIG_DEST ... \x1b[39;49;00m\n"
rm $TMUX_CONFIG_DEST
sudo -u ${SUDO_USER} ln -fs ${TMUX_CONFIG_SOURCE} ${TMUX_CONFIG_DEST}
# }}}
