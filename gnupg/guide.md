* On install:
GnuPG, when run on hosts without IPv6 connectivity, may fail to connect to
dual-stack hkp servers [1]. As a workaround, add

disable-ipv6

to

/usr/local/etc/dirmngr.conf

[1] https:/dev.gnupg.org/

