#!/bin/bash

INET_IF="ppp0"

INET_TCP_PORTS=(
	51413 # bittorrent
)

check_if_root()
{
	if [ "$EUID" -ne "0" ]; then
		echo "Please run me as root"
		exit 1
	fi
}

clear_rules()
{
	echo "Clearing iptables rules..."

	iptables -F
	iptables -X
	iptables -t nat -F
	iptables -t nat -X
	iptables -t mangle -F
	iptables -t mangle -X
	iptables -t raw -F
	iptables -t raw -X
	iptables -t security -F
	iptables -t security -X
}

create_user_chains()
{
	echo "Creating user-defined chains..."

	iptables -N tcp
	iptables -N udp
}

set_default_policies()
{
	echo "Setting default policies..."

	iptables -P INPUT DROP
	iptables -P FORWARD DROP
	iptables -P OUTPUT ACCEPT
}

set_inet_rules()
{
	echo "Setting iptables rules for Internet access..."

	# allow any established outgoing connections to receive replies from other hosts
	iptables -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

	# accept all traffic that comes to the loopback interface
	iptables -A INPUT -i lo -j ACCEPT

	# drop all invalid packets
	iptables -A INPUT -m conntrack --ctstate INVALID -j DROP

	# allow incoming ICMP echo requests (pings)
	iptables -A INPUT -p icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT

	# attach user-defined TCP and UDP chains to the INPUT chain to handle all new incoming connections
	iptables -A INPUT -p tcp --syn -m conntrack --ctstate NEW -j tcp
	iptables -A INPUT -p udp -m conntrack --ctstate NEW -j udp

	# open TCP ports
	for PORT in "${INET_TCP_PORTS[@]}"; do
		iptables -A tcp -p tcp --dport "$PORT" -j ACCEPT
	done

	# reject all other TCP/UDP packets with TCP RESET or ICMP port unreachable messages
	iptables -A INPUT -p tcp -j REJECT --reject-with tcp-reset
	iptables -A INPUT -p udp -j REJECT --reject-with icmp-port-unreachable

	# reject all remaining incoming traffic with ICMP protocol unreachable messages
	iptables -A INPUT -j REJECT --reject-with icmp-proto-unreachable
}

save_rules()
{
	echo "Saving iptables rules..."

	iptables-save > /etc/iptables/iptables.rules
}

main()
{
	check_if_root
	clear_rules
	create_user_chains
	set_default_policies
	set_inet_rules
	save_rules
}

main
