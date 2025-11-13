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
EMACS_CONF_DEST_DIR="${HOME}/.local/share/emacs.d/my.emacs.d"
EMACS_INSTALL_DIR="${HOME}/.local"
EMACS_REPO_DIR="${HOME}/Repos/emacs"
# EMACS_BRANCH_NAME="emacs-29"
EMACS_BRANCH_NAME="master"
BACKUP_DIR="${HOME}/backup"
EMACS_COMMIT="000f919b3c7"

##* install xorg

if  hash xorg 2>/dev/null; then
    echo -e "\n\x1b[33;01m xorg is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[33;01m Installing xorg and supporting packages ...  \x1b[39;49;00m\n" && sleep 1
    # !!! sound: install alsa-utils
    apt-get install -y xorg xattr xinput firefox-esr \
            feh mupdf zathura zathura-djvu hunspell \
            aspell graphviz r-base r-base-dev mpv alsa-utils
    # Org image resizing requires imagemagick
    # you need to specify compile config option --with-imagemagick
    apt-get install -y imagemagick libmagick++-dev libmagickwand-dev libmagickcore-dev
    echo -e "\n\x1b[33;01m Installing LaTeX, downloads over 2GB files  ...  \x1b[39;49;00m\n" && sleep 1
    apt-get install -y texlive-full dvipng
    # tidyverse of R requires next dependencies on Debian 10 (buster)
    apt-get install -y libssl-dev libcurl4-openssl-dev
    apt-get install -y silversearcher-ag ripgrep pass
    apt-get install -y lm-sensors upower ispell dictionaries-common iamerican
    apt-get install -y fonts-noto-color-emoji
fi

# link .XResources --------------------------------------------------------{{{
XRESOURCES_SOURCE="${SCRIPT_DIR}/XResources"
XRESOURCES_DEST="${HOME}/.XResources"
if [ -f $XRESOURCES_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XRESOURCES_SOURCE to $XRESOURCES_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XRESOURCES_DEST" ]; then
        rm $XRESOURCES_DEST
    fi
    if [ -h "$XRESOURCES_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XRESOURCES_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XRESOURCES_SOURCE $XRESOURCES_DEST
else
    echo -e "\n\x1b[31;01m ${XRESOURCES_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link .Xmodmap -----------------------------------------------------------{{{
XMODMAP_SOURCE="${SCRIPT_DIR}/Xmodmap"
XMODMAP_DEST="${HOME}/.Xmodmap"
if [ -f $XMODMAP_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XMODMAP_SOURCE to $XMODMAP_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XMODMAP_DEST" ]; then
        rm $XMODMAP_DEST
    fi
    if [ -h "$XMODMAP_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XMODMAP_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XMODMAP_SOURCE $XMODMAP_DEST
else
    echo -e "\n\x1b[31;01m ${XMODMAP_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link .xinitrc -----------------------------------------------------------{{{
XINIT_SOURCE="${SCRIPT_DIR}/xinitrc"
XINIT_DEST="${HOME}/.xinitrc"
if [ -f $XINIT_SOURCE ];
then
    echo -e "\n\x1b[33;01m Linking $XINIT_SOURCE to $XINIT_DEST ... \x1b[39;49;00m\n"
    if [ -f "$XINIT_DEST" ]; then
        rm $XINIT_DEST
    fi
    if [ -h "$XINIT_DEST" ]; then # -h, true if file exist and a symbolic link.
        rm $XINIT_DEST
    fi
	sudo -u ${SUDO_USER} ln -s $XINIT_SOURCE $XINIT_DEST
else
    echo -e "\n\x1b[31;01m ${XINIT_SOURCE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi
# }}}

# link .zathurarc ---------------------------------------------------------{{{
SOURCE_ZATHURA_CONF_FILE="${SCRIPT_DIR}/zathurarc"
DEST_ZATHURA_CONF_DIR="${HOME}/.config/zathura"
DEST_ZATHURA_CONF_FILE="${DEST_ZATHURA_CONF_DIR}/zathurarc"
if [ -f $SOURCE_ZATHURA_CONF_FILE ];
then
    echo -e "\n\x1b[33;01m Linking $SOURCE_ZATHURA_CONF_FILE to $DEST_ZATHURA_CONF_FILE ... \x1b[39;49;00m\n"
    sudo -u ${SUDO_USER} mkdir -p $DEST_ZATHURA_CONF_DIR
    if [ -f "$DEST_ZATHURA_CONF_FILE" ]; then
        rm $DEST_ZATHURA_CONF_FILE
    fi
    if [ -h "$DEST_ZATHURA_CONF_FILE" ]; then # -h, true if file exist and a symbolic link.
        rm $DEST_ZATHURA_CONF_FILE
    fi
	sudo -u ${SUDO_USER} ln -s $SOURCE_ZATHURA_CONF_FILE $DEST_ZATHURA_CONF_FILE
else
    echo -e "\n\x1b[31;01m ${SOURCE_ZATHURA_CONF_FILE} does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

##* install emacs

# consider installing the latest version, follow instructions in the link
# https://www.emacswiki.org/emacs/EmacsSnapshotAndDebian

# if hash emacs 2>/dev/null && [ -d "${EMACS_CONF_DEST_DIR}" ]; then
if hash emacs 2>/dev/null; then
    echo -e "\n\x1b[33;01m emacs is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
    # google search cli, need install pup, recorde, jq
    echo -e "\n\x1b[33;01m Installing Emacs ... \x1b[39;49;00m\n" && sleep 1
    # seem like too much work
    # https://github.com/Bugswriter/tuxi
    # curl -sL "https://raw.githubusercontent.com/Bugswriter/tuxi/main/tuxi" -o ~/.local/bin/tuxi
    # chmod +x ~/.local/bin/tuxi

    # echo -e "\n\x1b[33;01m installing emacs docs ...  \x1b[39;49;00m\n" && sleep 1
    # To insfall docs,
    # You may need to include non-free repository to /etc/apt/sources.list
    # deb http://deb.debian.org/debian/ buster main non-free contrib
    # deb-src http://deb.debian.org/debian/ buster main non-free contrib
    # apt-get install -y emacs-common-non-dfsg

    echo -e "\n\x1b[33;01m installing packages for compiling ...  \x1b[39;49;00m\n" && sleep 1
    # # tools for compiling
    # apt-get install -y autoconf make gcc texinfo xorg-dev libgtk-3-dev libgccjit-10-dev cmake libtool-bin\
    #         libjpeg-dev libgif-dev libpng-dev libtiff-dev libgnutls28-dev libtinfo-dev

    # tools for compiling
    apt-get install -y autoconf make gcc texinfo \
            cmake libtool-bin \
            libjpeg-dev libxpm-dev libgif-dev libpng-dev libtiff-dev librsvg2-dev \
            libgnutls28-dev libtinfo-dev \
            libgtk-3-dev \
            libwebkit2gtk-4.1-dev \
            libcairo2-dev libharfbuzz-dev
            #libgccjit-12-dev
            #libX11-dev libXpm-dev libxaw3dxft8-dev libxaw3dxft8-dev \
            #libwxgtk3.0-gtk3-dev \
            #libwxgtk-media3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev \
            #libwxgtk3.0-gtk3-dev \

    #        [ ! "0" -eq "$?" ] && \
    #        echo -e "\n\x1b[31;01m Failed to install packages!  \x1b[39;49;00m\n" && exit 1

    if [ -d $EMACS_REPO_DIR ];
    then
        echo -e "\n\x1b[33;01m Updating Emacs git repository ...  \x1b[39;49;00m\n" && sleep 1
        cd $EMACS_REPO_DIR
        sudo -u $SUDO_USER git pull
        sudo -u $SUDO_USER git checkout $EMACS_BRANCH_NAME
        sudo -u $SUDO_USER git checkout $EMACS_COMMIT

        # problems with automatically generated files from previos builds:
        # Some files in $EMACS_REPO_DIR/lisp will NEED TO BE UPDATED to reflect
        # new autoloaded functions. Otherwise,
        # you may see errors (rather than warnings) about undefined
        # lisp functions during compilation or build failures related to
        # these files (e.g., "required feature 'some-feature' was not provided.
        # This did not help me.
        #echo -e "\n\x1b[33;01m Updating $EMACS_REPO_DIR/lisp files from prev build ...  \x1b[39;49;00m\n" && sleep 1
        #cd $EMACS_REPO_DIR/lisp
        #sudo -u $SUDO_USER make autoloads
        #cd $EMACS_REPO_DIR

       echo -e "\n\x1b[33;01m Clean repository from files left from prev build ...  \x1b[39;49;00m\n" && sleep 1
       # !!! compiled emacs files prom previous build may contain code that
       # would conflict with new version. For example, old package may require
       # packages that are not in the source code any more. To stop that, remove all files
       # that do not belong to the repository
       sudo -u $SUDO_USER  git clean -fdx
    else
        echo -e "\n\x1b[33;01m Cloning Emacs git repository ...  \x1b[39;49;00m\n" && sleep 1
        sudo -u $SUDO_USER mkdir -p "$(dirname "$EMACS_REPO_DIR")"
        # sudo -u $SUDO_USER git clone git://git.savannah.gnu.org/emacs.git "$EMACS_REPO_DIR"
        sudo -u $SUDO_USER git clone -b master git://git.savannah.gnu.org/emacs.git "$EMACS_REPO_DIR"
        cd $EMACS_REPO_DIR
    fi

    ## From
    ## ./configure --help
    
    ## By default, `make install' will install all the files in
    ## `/usr/local/bin', `/usr/local/lib' etc.  You can specify
    ## an installation prefix other than `/usr/local' using `--prefix',
    ## for instance `--prefix=$HOME'.
    
    ## For better control, use the options below.

    ## --bindir=DIR            user executables [EPREFIX/bin]
    ## --sbindir=DIR           system admin executables [EPREFIX/sbin]
    ## --libexecdir=DIR        program executables [EPREFIX/libexec]
    ## --sysconfdir=DIR        read-only single-machine data [PREFIX/etc]
    ## --sysconfdir=DIR        read-only single-machine data [PREFIX/etc]
    ## --sharedstatedir=DIR    modifiable architecture-independent data [PREFIX/com]
    ## --localstatedir=DIR     modifiable single-machine data [PREFIX/var]
    ## --runstatedir=DIR       modifiable per-process data [LOCALSTATEDIR/run]
    ## --libdir=DIR            object code libraries [EPREFIX/lib]
    ## --includedir=DIR        C header files [PREFIX/include]
    ## --oldincludedir=DIR     C header files for non-gcc [/usr/include]
    ## --datarootdir=DIR       read-only arch.-independent data root [PREFIX/share]
    ## --datadir=DIR           read-only architecture-independent data [DATAROOTDIR]
    ## --infodir=DIR           info documentation [DATAROOTDIR/info]
    ## --localedir=DIR         locale-dependent data [DATAROOTDIR/locale]
    ## --mandir=DIR            man documentation [DATAROOTDIR/man]
    ## --docdir=DIR            documentation root [DATAROOTDIR/doc/emacs]
    ## --htmldir=DIR           html documentation [DOCDIR]
    ## --dvidir=DIR            dvi documentation [DOCDIR]
    ## --pdfdir=DIR            pdf documentation [DOCDIR]
    ## --psdir=DIR             ps documentation [DOCDIR]

    ## running./autogen.sh is needed to enagle
    ## ./configure options
    ## Another option is to specify
    ## config options is when running make,
    ## for example:
    ## make configure="--prefix=${HOME}/.local CFLAG='-O0 -g3'"
    sudo -u $SUDO_USER ./autogen.sh
    sudo -u $SUDO_USER ./configure \
         --prefix="$EMACS_INSTALL_DIR" \
         --bindir="$EMACS_INSTALL_DIR/bin" \
         --libexecdir="$EMACS_INSTALL_DIR/libexec" \
         --libexecdir="$EMACS_INSTALL_DIR/libexec" \
         --sysconfdir="$EMACS_INSTALL_DIR/etc" \
         --runstatedir="$EMACS_INSTALL_DIR/run" \
         --datarootdir="$EMACS_INSTALL_DIR/share" \
         --datadir="$EMACS_INSTALL_DIR/share" \
         --infodir="$EMACS_INSTALL_DIR/share/info" \
         --localedir="$EMACS_INSTALL_DIR/share/locale" \
         --mandir="$EMACS_INSTALL_DIR/share/man" \
         --docdir="$EMACS_INSTALL_DIR/share/doc/emacs" \
         --with-native-compilation \
         --with-sound=no \
         --with-cairo \
         --with-harfbuzz \
         --with-x \
         --with-x-toolkit=yes \
         --with-xft \
         --with-mailutils \
         --without-toolkit-scroll-bars \
         --with-xwidgets \
         --disable-ns-self-contained \
         --with-xml2 \
         --with-xim \
         --with-imagemagick \
         --with-tree-sitter \
         --without-compress-install \
         --disable-ns-self-contained # respect --prefix
    sudo -u $SUDO_USER make -j$(nproc)
    # why sometime it wants to use "/usr/local/share/info"
    # when installing and then giving
    # cannot create directory ‘/usr/local/share/info Permission denied
    # whereas --prefix="$EMACS_INSTALL_DIR" is given to use other location?
    sudo -u $SUDO_USER make install
    #make clean
    #apt-get install -y emacs
fi

#exit 0

#* link files

#** link files: init.el

INIT_SOURCE_PATH="${SCRIPT_DIR}/init.el"
INIT_DEST_PATH="${EMACS_CONF_DEST_DIR}/init.el"

if [ -f $INIT_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $INIT_SOURCE_PATH to $INIT_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$INIT_DEST_PATH" ]; then
        rm $INIT_DEST_PATH
    fi
    if [ -h "$INIT_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $INIT_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $INIT_SOURCE_PATH $INIT_DEST_PATH
else
    echo -e "\n\x1b[31;01m $INIT_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

#** link files: config

CONF_SOURCE_DIR="${SCRIPT_DIR}/config"
CONF_DEST_DIR="${EMACS_CONF_DEST_DIR}/config"

if [ -d $CONF_SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $CONF_SOURCE_DIR to $CONF_DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$CONF_DEST_DIR" ]; then
        rm -r $CONF_DEST_DIR
    fi
    if [ -h "$CONF_DEST_DIR" ]; then  # -h, true if file exist and a symbolic link.
        rm -r $CONF_DEST_DIR
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $CONF_SOURCE_DIR $CONF_DEST_DIR
else
    echo -e "\n\x1b[31;01m $CONF_SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

#** link files: lisp

LISP_SOURCE_DIR="${SCRIPT_DIR}/lisp"
LISP_DEST_DIR="${EMACS_CONF_DEST_DIR}/lisp"

if [ -d $LISP_SOURCE_DIR ];
then
    echo -e "\n\x1b[33;01m Linking $LISP_SOURCE_DIR to $LISP_DEST_DIR ... \x1b[39;49;00m\n"
    if [ -d "$LISP_DEST_DIR" ]; then
        rm -r $LISP_DEST_DIR
    fi
    if [ -h "$LISP_DEST_DIR" ]; then  # -h, true if file exist and a symbolic link.
        rm -r $LISP_DEST_DIR
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $LISP_SOURCE_DIR $LISP_DEST_DIR
else
    echo -e "\n\x1b[31;01m $LISP_SOURCE_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

GITIGNORE_SOURCE_PATH="${SCRIPT_DIR}/../.gitignore"
GITIGNORE_DEST_PATH="${EMACS_CONF_DEST_DIR}/.gitignore"

if [ -f $GITIGNORE_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $GITIGNORE_SOURCE_PATH to $GITIGNORE_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$GITIGNORE_DEST_PATH" ]; then
        rm $GITIGNORE_DEST_PATH
    fi
    if [ -h "$GITIGNORE_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $GITIGNORE_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $GITIGNORE_SOURCE_PATH $GITIGNORE_DEST_PATH
else
    echo -e "\n\x1b[31;01m $GITIGNORE_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

ESHELL_DIR_SOURCE_PATH="${BACKUP_DIR}/emacs/eshell"
ESHELL_DIR_DEST_PATH="${EMACS_CONF_DEST_DIR}/eshell"

if [ -d $ESHELL_DIR_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $ESHELL_DIR_SOURCE_PATH to $ESHELL_DIR_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -d "$ESHELL_DIR_DEST_PATH" ]; then
        rm -r $ESHELL_DIR_DEST_PATH
    fi
    if [ -h "$ESHELL_DIR_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $ESHELL_DIR_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $ESHELL_DIR_SOURCE_PATH $ESHELL_DIR_DEST_PATH
else
    echo -e "\n\x1b[31;01m $ESHELL_DIR_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

HISTORY_SOURCE_PATH="${BACKUP_DIR}/emacs/history"
HISTORY_DEST_PATH="${EMACS_CONF_DEST_DIR}/history"

if [ -f $HISTORY_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $HISTORY_SOURCE_PATH to $HISTORY_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$HISTORY_DEST_PATH" ]; then
        rm $HISTORY_DEST_PATH
    fi
    if [ -h "$HISTORY_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $HISTORY_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $HISTORY_SOURCE_PATH $HISTORY_DEST_PATH
else
    echo -e "\n\x1b[31;01m $HISTORY_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

SAVEHISTORY_SOURCE_PATH="${BACKUP_DIR}/emacs/savehist"
SAVEHISTORY_DEST_PATH="${EMACS_CONF_DEST_DIR}/savehist"

if [ -f $SAVEHISTORY_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $SAVEHISTORY_SOURCE_PATH to $SAVEHISTORY_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$SAVEHISTORY_DEST_PATH" ]; then
        rm $SAVEHISTORY_DEST_PATH
    fi
    if [ -h "$SAVEHISTORY_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $SAVEHISTORY_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $SAVEHISTORY_SOURCE_PATH $SAVEHISTORY_DEST_PATH
else
    echo -e "\n\x1b[31;01m $SAVEHISTORY_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

RECENTF_SOURCE_PATH="${BACKUP_DIR}/emacs/recentf"
RECENTF_DEST_PATH="${EMACS_CONF_DEST_DIR}/recentf"

if [ -f $RECENTF_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $RECENTF_SOURCE_PATH to $RECENTF_DEST_PATH ... \x1b[39;49;00m\n"
    if [ -f "$RECENTF_DEST_PATH" ]; then
        rm $RECENTF_DEST_PATH
    fi
    if [ -h "$RECENTF_DEST_PATH" ]; then  # -h, true if file exist and a symbolic link.
        rm $RECENTF_DEST_PATH
    fi
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
    sudo -u ${SUDO_USER} ln -s $RECENTF_SOURCE_PATH $RECENTF_DEST_PATH
else
    echo -e "\n\x1b[31;01m $RECENTF_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

# list of multicursor commands allowed for multiplo cursors
MC_LISTS_SOURCE_PATH="${BACKUP_DIR}/emacs/.mc-lists.el"
MC_LISTS_DEST_PATH="${EMACS_CONF_DEST_DIR}/.mc-lists.el"

if [ -f $MC_LISTS_SOURCE_PATH ];
then
    echo -e "\n\x1b[33;01m Linking $MC_LISTS_SOURCE_PATH to $MC_LISTS_DEST_PATH ... \x1b[39;49;00m\n"
    sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
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
#     sudo -u ${SUDO_USER} mkdir -p $EMACS_CONF_DEST_DIR
#     sudo -u ${SUDO_USER} ln -s $MC_LISTS_SOURCE_PATH $MC_LISTS_DEST_PATH
# else
#     echo -e "\n\x1b[31;01m $MC_LISTS_SOURCE_PATH does not exist. Quiting ... \x1b[39;49;00m\n"
# 	exit 1
# fi


# ** link EMACS_CONF_DIR (${HOME}/.emacs.d) to EMACS_CONF_DEST_DIR
EMACS_CONF_DIR="${HOME}/.emacs.d"

if [ -d $EMACS_CONF_DEST_DIR ];
then
    echo -e "\n\x1b[33;01m Removing default $EMACS_CONF_DIR ... \x1b[39;49;00m\n"
    #if [ -d "$EMACS_CONF_DIR" ]; then
    #    rm -r $EMACS_CONF_DIR
    #fi
    #if [ -h "$EMACS_CONF_DIR" ]; then  # -h, true if file exist and a symbolic link.
    #    rm $EMACS_CONF_DIR
    #fi
    echo -e "\n\x1b[33;01m Linking $EMACS_CONF_DEST_DIR to $EMACS_CONF_DIR ... \x1b[39;49;00m\n"
    sudo -u ${SUDO_USER} ln -fs $EMACS_CONF_DEST_DIR $EMACS_CONF_DIR
else
    echo -e "\n\x1b[31;01m $EMACS_CONF_DEST_DIR does not exist. Quiting ... \x1b[39;49;00m\n"
	exit 1
fi

##** install qutebrowser
if hash qutebrowser 2>/dev/null && [ -d "${HOME}/.config/qutebrowser" ]; then
    echo -e "\n\x1b[33;01m qutebrowser is installed, not installing or upgrading.\x1b[39;49;00m\n" && sleep 1
else
#     echo -e "\n\x1b[33;01m Installing, configuring qutebrowser ...  \x1b[39;49;00m\n" && sleep 1
    QB_SETUPDIR=${SCRIPT_DIR}/../qutebrowser
    QB_SETUPFILE=${QB_SETUPDIR}/setup_qb.sh
    cd ${QB_SETUPDIR}
    source ${QB_SETUPFILE}
fi

SCRIPT_DIR=$SCRIPT_DIR_OLD
