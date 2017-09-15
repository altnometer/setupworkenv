#! /bin/bash
set -e

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
FILE_RULESV4='/etc/iptables/rules.v4';
FILE_RULESV6='/etc/iptables/rules.v6';

function setup_iptables_for_newserver {
echo -e "\n\x1b[33;01m Starting setting up iptales for fresh server installation ... \x1b[39;49;00m\n" && sleep 3

if [[ $EUID -ne 0 ]]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    return 1
fi
if [ $HOME  = '/root' ]; then
    echo -e "\n\x1b[31;01m Run this script with 'sudo -E' \x1b[39;49;00m\n"
    return 1
fi

if [ "$(( $(date +%s) - $(stat -c %Z /var/cache/apt/pkgcache.bin 2>/dev/null || echo '0') ))" -ge 600 ]; then
    echo -e "\n\x1b[33;01m Running apt-get update ... \x1b[39;49;00m\n" && sleep 1
    apt-get update
fi

echo -e "\n\x1b[33;01m Install iptables-persistent ... \x1b[39;49;00m\n" && sleep 1
apt-get install -y iptables-persistent


cp ${SCRIPTDIR}/rules.v4 ${FILE_RULESV4}
## Firewall rules are only when creating an image for machine instances.
## The ip address is not going to be the correct one. Do not use it here.
#if [ ! -z $( echo $( hostname -I ) | awk '{ print $1 }' ) ]; then
#    SERVER_IP=$( echo $( hostname -I ) | awk '{ print $1 }' );
#    ADD_STR=$( grep '^#-A INPUT -i eth0 -s ${SERVER_IP} -j DROP$' ${FILE_RULESV4});
#    ADD_STR=$( echo ${ADD_STR} | sed -e "s/\${SERVER_IP}/${SERVER_IP}/g" | sed "s/^#//")
#    sed -i "/\${SERVER_IP}/a${ADD_STR}" ${FILE_RULESV4}
#fi
#if [ ! -z $( echo $( hostname -I ) | awk '{ print $2 }' ) ]; then
#    SERVER_IP=$( echo $( hostname -I ) | awk '{ print $2 }' )
#    ADD_STR=$( grep '^#-A INPUT -i eth0 -s ${SERVER_IP} -j DROP$' ${FILE_RULESV4})
#    ADD_STR=$( echo ${ADD_STR} | sed -e "s/\${SERVER_IP}/${SERVER_IP}/g" | sed "s/^#//")
#    sed -i "/\${SERVER_IP}/a${ADD_STR}" ${FILE_RULESV4}
#fi
echo -e "\n\x1b[33;01m Testing iptables ${FILE_RULESV4} ... \x1b[39;49;00m\n" && sleep 1
if iptables-restore -t < ${FILE_RULESV4} >/dev/null 2>&1; then
    echo -e "\n\x1b[32;01m Passed testing ${FILE_RULESV4} \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[31;01m Failed testing ${FILE_RULESV4}, exiting script!  \x1b[39;49;00m\n" && sleep 1
    return 1
fi

echo -e "\n\x1b[33;01m Testing iptables ${FILE_RULESV6} ... \x1b[39;49;00m\n" && sleep 1
cp ${SCRIPTDIR}/rules.v6 /etc/iptables/
if ip6tables-restore -t < ${FILE_RULESV6} >/dev/null 2>&1; then
    echo -e "\n\x1b[32;01m Passed testing ${FILE_RULESV6} \x1b[39;49;00m\n" && sleep 1
else
    echo -e "\n\x1b[31;01m Failed testing ${FILE_RULESV6}, exiting script!  \x1b[39;49;00m\n" && sleep 1
    return 1
fi

service netfilter-persistent reload
echo -e "\n\x1b[33;01m Current iptables rules.v4 \x1b[39;49;00m\n" && sleep 1
iptables -S
echo -e "\n\x1b[33;01m Current iptables rules.v6 \x1b[39;49;00m\n" && sleep 1
ip6tables -S
return 0
}
