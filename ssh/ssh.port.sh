#!/bin/sh
##suppose we got an erlang distributed node started with net_kernel:start([fara]).
# ssh -p <target-listening-on-port> <user>@<hostname>
# `<user>@<hostname>´ is called the (unique) identifier of the node.
# In distributed Erlang, ´epmd´ daemon process that listens for incoming connection requests on port 4369, mapping them to the listening port of the node that is being connected to.

ssh -p 4369 fara@qd.local
