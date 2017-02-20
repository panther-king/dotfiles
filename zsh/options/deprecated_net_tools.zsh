# http://blog.livedoor.jp/sonots/archives/38589335.html
net_tools_deprecated_msg () {
    echo 'Do not use deprecated "net-tools" commands'
}

arp () {
    net_tools_deprecated_msg
    echo 'Use `ip n`'
}

ifconfig () {
    net_tools_deprecated_msg
    echo 'Use `ip a`, `ip link`, `ip -s link`'
}

iptunnel () {
    net_tools_deprecated_msg
    echo 'Use `ip tunnel`'
}

iwconfig () {
    net_tools_deprecated_msg
    echo 'Use `iw`'
}

nameif () {
    net_tools_deprecated_msg
    echo 'Use `ip link`, `ifrename`'
}

netstat () {
    net_tools_deprecated_msg
    echo 'Use `ss`, `ip route` (for netstat -r), `ip -s link` (for netstat -i), `ip maddr` (for netstat -g)'
}

route () {
    net_tools_deprecated_msg
    echo 'Use `ip r`'
}
