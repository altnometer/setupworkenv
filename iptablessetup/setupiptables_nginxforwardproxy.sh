#! /bin/bash
set -e

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$SCRIPTDIR/setupnewserveriptables.sh"

function setup_iptables_for_nginxforwardproxy {
echo -e "\n\x1b[33;01m Starting setting up iptales for nginxforwardproxy ... \x1b[39;49;00m\n" && sleep 1

if [[ $EUID -ne 0 ]]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    return 1
fi
if [ $HOME  = '/root' ]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    return 1
fi

if  service netfilter-persistent restart >/dev/null 2>&1; then
    echo -e "\n\x1b[32;01m Installed 'iptables-persistent'  \x1b[39;49;00m\n"
else
    echo -e "\n\x1b[31;01m 'service netfilter-persistent save' command failed. \x1b[39;49;00m"
    echo -e "\x1b[31;01m Install 'iptables-persistent' and try again. \x1b[39;49;00m\n"
    return 1
fi

ADD_AFTER_THIS_REGEX_STR='^#-A TCP -p tcp --dport 80 -j ACCEPT$'
ADD_THIS_STR='-A TCP -p tcp --dport 80 -j ACCEPT'
sed -i "/${ADD_AFTER_THIS_REGEX_STR}/a${ADD_THIS_STR}" ${FILE_RULESV4}

echo -e "\n\x1b[33;01m Testing iptables ${FILE_RULESV4} ... \x1b[39;49;00m\n" && sleep 1
if iptables-restore -t < ${FILE_RULESV4} >/dev/null 2>&1; then
    echo -e "\n\x1b[32;01m Passed testing ${FILE_RULESV4} \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[31;01m Failed testing ${FILE_RULESV4}, exiting script!  \x1b[39;49;00m\n" && sleep 1
    return 1
fi

#echo -e "\n\x1b[33;01m Testing iptables ${FILE_RULESV6} ... \x1b[39;49;00m\n" && sleep 1
#if ip6tables-restore -t < ${FILE_RULESV6} >/dev/null 2>&1; then
#    echo -e "\n\x1b[32;01m Passed testing ${FILE_RULESV6} \x1b[39;49;00m\n" && sleep 1
#else
#    echo -e "\n\x1b[31;01m Failed testing ${FILE_RULESV6}, exiting script!  \x1b[39;49;00m\n" && sleep 1
#    return 1
#fi
echo -e "\n\x1b[33;01m Reload iptables rules ... \x1b[39;49;00m\n" && sleep 1
service netfilter-persistent reload > /dev/null
echo -e "\n\x1b[33;01m Current iptables rules.v4 \x1b[39;49;00m\n" && sleep 1
iptables -S
#echo -e "\n\x1b[33;01m Current iptables rules.v6 \x1b[39;49;00m\n" && sleep 1
#ip6tables -S
return 0
}
