#!/bin/bash
 set -e
# This scrip should install docker on debian.

# vars --------------------------------------------------------------------{{{
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
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

# setup docker ------------------------------------------------------------{{{

# remove old version
# https://docs.docker.com/install/linux/docker-ce/debian/#install-using-the-repository
# apt-get remove docker docker-engine docker.io containerd runc
# Install packages to allow apt to use a repository over HTTPS:

if hash docker 2>/dev/null; then
    echo -e "\n\x1b[33;01m docker is installed, not installing. \x1b[39;49;00m\n" && exit 0
else
    echo -e "\n\x1b[33;01m Installing docker ...  \x1b[39;49;00m\n" && sleep 1

    # installing {{{2
    apt-get install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg2 \
        software-properties-common

    # Add Docker’s official GPG key:
    curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

    # Verify that you now have the key with the fingerprint
    # 9DC8 5822 9FC7 DD38 854A E2D8 8D81 803C 0EBF CD88,
    # by searching for the last 8 characters of the fingerprint.

    if apt-key fingerprint 0EBFCD88 2>/dev/null | \
        awk '{ if (NR==2) print $1 $2 $3 $4 $5 $6 $7 $8 $9 $10}' | \
        grep -q "9DC858229FC7DD38854AE2D88D81803C0EBFCD88";
    then
        echo -e "\n\x1b[33;01m apt-key fingerprint correct, continue...  \x1b[39;49;00m\n"
    else
        echo -e "\n\x1b[31;01m apt-key fingerprint not correct, quiting...  \x1b[39;49;00m\n"
        exit
    fi

    add-apt-repository \
       "deb [arch=amd64] https://download.docker.com/linux/debian \
       $(lsb_release -cs) \
       stable"

    apt-get update
    apt-get install -y docker-ce
    exit 0
    # }}}2
fi
# }}}
