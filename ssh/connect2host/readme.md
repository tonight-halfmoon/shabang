# Connect to a Host via SSH

To be able to remotely connect to a host machine via ssh, evaluate the following

- On your machine
Generate a SSH private and public keys. Command `ssh-keygen` is the starting point.
For some hosting machines they aquire and identity ssh public key. This is accomplished by leaving the key name to be the default.
In other words, when you evaluate command `ssh-keygen` and it asks to providing a name for the new file, then leave the default suggested. Just hit Enter key without typing anything. See the following output example:

```
Generating public/private rsa key pair.
Enter file in which to save the key (/home/<username>/.ssh/id_rsa):

```

The previous message and input prompt will be shown once the user evaluate the following command as described above.

```
$ ssh-keygen
```

After that, the same program will ask for a passphrase for the private key. Type your desired passphrase and memorise it because you will need it every time you access the host machine to get authorised.


At this point one needs to copy the public key to the hosting machine.

## Copying SSH Public Keu to the Host Machine

### Option 1
In case you have an authorised access to the host mahcine with a different username then, use that username to access the host machine and copy the contents of the SSH public key file, as follows:

```
$ cat id_rsa.pub >> /home/<username-at-host>/.ssh/authorized_keys
```

!Note: Now on, you will use the same <username-at-host> when trying to access the host machine with `ssh`.
For example, if the <username-at-host> is called `april`, then command above should be 

```
$ cat id_rsa.pub >> /home/april/.ssh/authorized_keys
```

At this point, you are ready to make an attempt to access the host machine. Suppose your username at your machine (not the host) is called 'june', then the command is as follows:

```
/home/june $ ssh april@<host-domain-name | host-IP>
```

At thie point SSH program will compare the public key found in its `authorized_keys` at the host and the one in your `.ssh` directory. SSH assures that is yours by relating that to the private key also found in your `.ssh` directory. Finally, the is confirmed once you provided the passphrase of your private key - if your have already typed one.

At this point you must have an authorised access to the host machine. 

## Verbose mode
In case your face some difficulties, you may use option `-v` to see the debug/log, as follows

```
$ ssh -v april@<host-domain-name| host-IP>
```

### Option 2

Directly copy the SSH public key to the host as follows

```
/home/june $ ssh-copy-id -i ~/.ssh/id_rsa.pub april@<host-domain-name | host-IP>
```

