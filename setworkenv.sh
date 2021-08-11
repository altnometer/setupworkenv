#!/bin/bash
# Set up a debian computer working environment.

set -e

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

# vars --------------------------------------------------------------------{{{
# source $HOME/redmoo/auct/setup/setup_conf/all_scripts_conf.sh
# which isn't cloned yet. So, they are redefined here, check
# that they have not changed.
# !!! ${HOME} must refer to /home/${SUDO_USER} rather than /root
REDMOO_DIR="${HOME}/redmoo"
REDMOO_PROJECT_DIR="${REDMOO_DIR}/setupworkenv"
# Change the public ip to the current value.
OOSSH_KEY_FOR_GIT_REPOSITORY="${HOME}/.ssh/github_ssh_key_altnometer"
# CLONE_REDMOO_FROM_THIS_REPOSITORY='lighthog@admin1:/home/lighthog/Depot/auct.git'
CLONE_REDMOO_FROM_THIS_REPOSITORY='git@github.com:altnometer/setupworkenv.git'

if [ ! -f "${SSH_KEY_FOR_GIT_REPOSITORY}" ]; then
    echo -e "\n\x1b[31;01m No ${SSH_KEY_FOR_GIT_REPOSITORY}. scp the keys before running the script. \x1b[39;49;00m\n"
    exit 1
fi
# }}}

# To this point:
# a username must be created,
# sudo must be installed and username should be
# run this command if it is not: # adduser username sudo

# git ---------------------------------------------------------------------{{{
if hash git 2>/dev/null; then
    echo -e "\n\x1b[33;01m git is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing git ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y git-core
    sudo -u ${SUDO_USER} git config --global user.email "${SUDO_USER}\@${HOSTNAME}"
    sudo -u ${SUDO_USER} git config --global user.name "${SUDO_USER} at ${HOSTNAME}"
    sudo -u ${SUDO_USER} git config --global push.default simple
fi
# }}}

# redmoo repo -------------------------------------------------------------{{{
if [ -d "${REDMOO_PROJECT_DIR}" ];
then
    echo -e "\n\x1b[33;01m ${REDMOO_PROJECT_DIR} directory exists, not cloning. \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m cloning project ... \x1b[39;49;00m\n" && sleep 1
    sudo -u ${SUDO_USER} mkdir -p ${REDMOO_DIR}
    cd ${REDMOO_DIR}
    #sudo -u ${SUDO_USER} git clone git+ssh://${CLONE_REDMOO_FROM_THIS_REPOSITORY}
    # sudo -u ${SUDO_USER} ssh-agent bash -c "ssh-add ${SSH_KEY_FOR_GIT_REPOSITORY}; git clone git+ssh://${CLONE_REDMOO_FROM_THIS_REPOSITORY}"
    sudo -u ${SUDO_USER} ssh-agent bash -c "ssh-add ${SSH_KEY_FOR_GIT_REPOSITORY}; git clone ${CLONE_REDMOO_FROM_THIS_REPOSITORY}"
fi

# Now the all_scripts_conf.sh is available from the cloned project,
# source it to make needed constatns available.
# source ${REDMOO_PROJECT_DIR}/setup/setup_conf/all_scripts_conf.sh
# }}}

# # neovim ------------------------------------------------------------------{{{
# if hash nvim 2>/dev/null && [ -d "${HOME}/.config/nvim" ]; then
#     echo -e "\n\x1b[33;01m neovim is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m Installing, configuring neovim ...  \x1b[39;49;00m\n" && sleep 1
#     VIMSETUPDIR=${REDMOO_PROJECT_DIR}/vimsetup
#     VIMSETUPFILE=${VIMSETUPDIR}/setup_neovim.sh
#     cd ${VIMSETUPDIR}
#     source ${VIMSETUPFILE}
# fi
# # }}}

# # vim ---------------------------------------------------------------------{{{
# if hash vim 2>/dev/null && [ -d "${HOME}/.vim" ]; then
#     echo -e "\n\x1b[33;01m vim is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m Installing, configuring vim ...  \x1b[39;49;00m\n" && sleep 1
#     VIMSETUPDIR=${REDMOO_PROJECT_DIR}/vimsetup
#     VIMSETUPFILE=${VIMSETUPDIR}/setup_vim.sh
#     cd ${VIMSETUPDIR}
#     source ${VIMSETUPFILE}
# fi
# # }}}

# zsh ---------------------------------------------------------------------{{{
echo -e "\n\x1b[33;01m Configuring shell ... \x1b[39;49;00m\n" && sleep 1
SHELLSETUPDIR=${REDMOO_PROJECT_DIR}/shell
SHELLSETUPFILE=${SHELLSETUPDIR}/setupzsh.sh
source ${SHELLSETUPFILE}
# }}}

# # tmux --------------------------------------------------------------------{{{
# echo -e "\n\x1b[33;01m Configuring tmux ... \x1b[39;49;00m\n" && sleep 1
# TMUXSETUPDIR=${REDMOO_PROJECT_DIR}/tmuxsetup
# TMUXSETUPFILE=${TMUXSETUPDIR}/setup_tmux.sh
# source ${TMUXSETUPFILE}
# # }}}

# # ranger ------------------------------------------------------------------{{{
# RANGERSETUPDIR=${REDMOO_PROJECT_DIR}/ranger
# RANGERCONFIGFILE=${RANGERSETUPDIR}/setup_ranger.sh
# cd ${RANGERSETUPDIR}
# source ${RANGERCONFIGFILE}
# # }}}

# # terminal ----------------------------------------------------------------{{{
# # should go to gui setup
# # setup st terminal emulator
# STSETUPDIR=${REDMOO_PROJECT_DIR}/term
# STSETUPFILE=${STSETUPDIR}/setup_st.sh
# cd ${STSETUPDIR}
# source ${STSETUPFILE}
# # }}}

# iftop -------------------------------------------------------------------{{{
#if ! hash /usr/sbin/iftop 2>/dev/null; then
#    echo -e "\n\x1b[33;01m Installing iftop...  \x1b[39;49;00m\n" && sleep 1
#    apt-get install -y iftop
#fi
# }}}

# GUI ---------------------------------------------------------------------{{{
# if hash nvim 2>/dev/null && [ -d "${HOME}/.config/nvim" ]; then
#     echo -e "\n\x1b[33;01m neovim is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
# else
#     echo -e "\n\x1b[33;01m Installing, configuring neovim ...  \x1b[39;49;00m\n" && sleep 1
    GUISETUPDIR=${REDMOO_PROJECT_DIR}/i3wm
    GUISETUPFILE=${GUISETUPDIR}/setup_GUI.sh
    cd ${GUISETUPDIR}
    source ${GUISETUPFILE}
# fi
# }}}

# # docker ------------------------------------------------------------------{{{
# DOCKERSETUPDIR=${REDMOO_PROJECT_DIR}/docker
# DOCKERSETUPFILE=${DOCKERSETUPDIR}/setup_docker.sh
# cd ${DOCKERSETUPDIR}
# source ${DOCKERSETUPFILE}
# # }}}

# # elixir ------------------------------------------------------------------{{{
# ELIXIRSETUPDIR=${REDMOO_PROJECT_DIR}/elixir
# ELIXIRSETUPFILE=${ELIXIRSETUPDIR}/setup_elixir.sh
# cd ${ELIXIRSETUPDIR}
# source ${ELIXIRSETUPFILE}
# # }}}

exit
source ${PROJECT_SETUP_DIR}/iptablessetup/setupnewserveriptables.sh
setup_iptables_for_newserver

#rm -f "${HOME}/setupredmoo_user.sh"
echo -e "\n\x1b[32;01m Completed function execution with no errors. \x1b[39;49;00m\n" && sleep 1
echo -e "\n\x1b[33;01m Logout and login as the same user for changes to take effect. \x1b[39;49;00m\n" && sleep 1
exit 0
