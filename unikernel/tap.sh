sudo ip tuntap del dev tap0 mode tap

sudo ip tuntap add dev tap0 user $USER mode tap
sudo ifconfig tap0 172.16.0.1  netmask 255.255.255.0
echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward
sudo iptables -t nat -A POSTROUTING -o wlp3s0 -j MASQUERADE
sudo iptables -I FORWARD 1 -i tap0 -j ACCEPT
sudo iptables -I FORWARD 1 -o tap0 -m state --state RELATED,ESTABLISHED -j ACCEPT
