#!/bin/bash
set -e

# cd to the folder before running
#https://github.com/altercation/solarized
if [ -z ${SUDO_USER} ]; then
    echo -e "\n\x1b[31;01m No \$SUDO_USER available, quiting ... \x1b[39;49;00m\n"
    exit 1
fi
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ZSH_CUSTOM_PLUG_DIR=${HOME}/.oh-my-zsh/custom/plugins
if hash zsh 2>/dev/null;
then
    echo -e "\n\x1b[33;01m zsh is installed, not installing or upgrading. \x1b[39;49;00m\n" && sleep 1
else 
    apt-get install -y zsh 
    chsh --shell $(which zsh) ${SUDO_USER}
    sudo -u ${SUDO_USER} sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    sudo -u ${SUDO_USER} git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM_PLUG_DIR}/zsh-autosuggestions
    sudo -u ${SUDO_USER} git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM_PLUG_DIR}/zsh-syntax-highlighting

fi

# install oh-my-zsh
# if [ -z "$DESKTOP_SESSION" -a "$DESKTOP_SESSION" == "lightdm-xsession" ];
# then
#     echo -e "$COL_GREEN seting up color scheme for xfce4 $COL_RESET"
#     file_conf_orig="${SCRIPT_DIR}/terminalrc"
#     dir_conf_dest="${HOME}/.config/Terminal"
#     file_conf_dest="${dir_conf_dest}/terminalrc"
#     sudo -u ${SUDO_USER} mkdir -pv "${dir_conf_dest}"
#     sudo -u ${SUDO_USER} cp "$file_conf_orig"  "$file_conf_dest"
# fi
ZSH_PRE_OH_MY_ZSH_SOURCE=${SCRIPT_DIR}/zshrc.pre-oh-my-zsh
ZSH_PRE_OH_MY_ZSH_DEST=${HOME}/.zshrc.pre-oh-my-zsh
echo -e "\n\x1b[33;01m linking .zshrc and .profile \x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/zshrc ${HOME}/.zshrc
sudo -u ${SUDO_USER} ln -fs ${SCRIPT_DIR}/profile ${HOME}/.profile
echo -e "\n\x1b[33;01m linking ${ZSH_PRE_OH_MY_ZSH_SOURCE} to ${ZSH_PRE_OH_MY_ZSH_DEST}\x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${ZSH_PRE_OH_MY_ZSH_SOURCE} ${ZSH_PRE_OH_MY_ZSH_DEST}
ZSH_CUSTOM_THEME_SOURCE=${SCRIPT_DIR}/my256.zsh-theme
ZSH_CUSTOM_THEME_DEST=${HOME}/.oh-my-zsh/custom/themes
echo -e "\n\x1b[33;01m linking ${ZSH_CUSTOM_THEME_SOURCE} to ${ZSH_CUSTOM_THEME_DEST}\x1b[39;49;00m\n" && sleep 1
sudo -u ${SUDO_USER} ln -fs ${ZSH_CUSTOM_THEME_SOURCE} ${ZSH_CUSTOM_THEME_DEST}
