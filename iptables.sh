#!/bin/bash

INET_IF="ppp0"
INET_TCP_PORTS=(
	51413 # BitTorrent
)

VM_IF="br0"
VM_SUBNET="192.168.13.0/24"
VM_TCP_PORTS=(
	139   # NetBIOS Session
	445   # SMB over TCP
	24800 # Synergy
)
VM_UDP_PORTS=(
	137   # NetBIOS Name Service
	138   # NetBIOS Datagram
)
VM_WHITELIST=(
	8.8.8.8            # Google Public DNS
	192.30.252.0/22    # GitHub
	185.199.108.0/22   # GitHub
	50.31.156.0/25     # Beanstalk
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
	iptables -N whitelist
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

	# allow already established incoming connections
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

set_vm_rules()
{
	echo "Setting iptables rules for virtual machines..."

	# allow already established connections in both directions
	iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

	# filter all outgoing Internet connections from virtual machines in user-defined whitelist chain
	iptables -A FORWARD -i "$VM_IF" -j whitelist

	# grant access only to whitelisted IP addresses
	for IP in "${VM_WHITELIST[@]}"; do
		iptables -A whitelist -d "$IP" -j ACCEPT
	done

	# open additional TCP/UDP ports but only for connections coming from virtual machines
	for PORT in "${VM_TCP_PORTS[@]}"; do
		iptables -A tcp -i "$VM_IF" -p tcp --dport "$PORT" -j ACCEPT
	done
	for PORT in "${VM_UDP_PORTS[@]}"; do
		iptables -A udp -i "$VM_IF" -p udp --dport "$PORT" -j ACCEPT
	done

	# reject all remaining forwarded traffic with ICMP host unreachable messages
	iptables -A FORWARD -j REJECT --reject-with icmp-host-unreachable

	# setup NAT for virtual machines
	iptables -t nat -A POSTROUTING -s "$VM_SUBNET" -o "$INET_IF" -j MASQUERADE

	# clamp MSS field in TCP packets to PMTU value (required for PPP connections)
	iptables -t mangle -A FORWARD -p tcp --tcp-flags SYN,RST SYN -j TCPMSS --clamp-mss-to-pmtu
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
	set_vm_rules
	save_rules
}

main
