#!/bin/bash
 set -e
# This scrip should install elixir on debian.

# vars --------------------------------------------------------------------{{{
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ELIXIR_PKG_SOURCE="https://github.com/elixir-lang/elixir/releases/download/v1.8.1/Precompiled.zip"
ELIXIR_DEST_DIR="${HOME}/.local/share/elixir"
ELIXIR_LOCAL_PKG_NAME="Precompiled.zip"
ELIXIR_PKG_PATH="${ELIXIR_DEST_DIR}/${ELIXIR_LOCAL_PKG_NAME}"
ELIXIR_LOCAL_BIN="${ELIXIR_DEST_DIR}/bin"
ELIXIR_DEST_BIN="${HOME}/.local/bin"
# }}}

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

# setup erlang ------------------------------------------------------------{{{
if hash erl 2>/dev/null; then
    echo -e "\n\x1b[33;01m erlang is installed, not installing. \x1b[39;49;00m\n"
else
    echo -e "\n\x1b[33;01m Installing erlang ...  \x1b[39;49;00m\n" && sleep 1
    # https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_21.2.5-1~debian~stretch_amd64.deb
    apt-get install -y erlang
fi
# }}}

# setup elixir ------------------------------------------------------------{{{
if hash iex 2>/dev/null; then
    echo -e "\n\x1b[33;01m elixir is installed, not installing. \x1b[39;49;00m\n" && exit 0
else
    # apt-get install -y elixir
    echo -e "\n\x1b[33;01m Installing elixir ...  \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} mkdir -p $ELIXIR_DEST_DIR && cd $_
    sudo -u ${SUDO_USER} curl -Lo $ELIXIR_PKG_PATH --create-dir $ELIXIR_PKG_SOURCE
    sudo -u ${SUDO_USER} unzip $ELIXIR_PKG_PATH
    sudo -u ${SUDO_USER} cp $ELIXIR_LOCAL_BIN/* $ELIXIR_DEST_BIN
fi
# }}}
