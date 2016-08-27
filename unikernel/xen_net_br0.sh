sudo brctl addbr br0
sudo ifconfig br0 10.0.0.1

echo 1 |sudo tee /proc/sys/net/ipv4/ip_forward
sudo iptables -t nat -A POSTROUTING -o wlp3s0 -j MASQUERADE
sudo iptables -I FORWARD 1 -i br0 -j ACCEPT
sudo iptables -I FORWARD 1 -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT

