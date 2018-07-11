if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[36;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=cxbxgxgxfxxxxxfhfghfhx
alias ls='ls -GFh'

export JAVA_HOME=$(/usr/libexec/java_home)
#export JAVA_OPTS="$JAVA_OPTS -Djavax.net.ssl.trustStore=/Users/amado/config/keystores/truststore.jks"
#export PATH=~/libs/apache-tomcat-7.0-81/bin:~/libs/apache-ant-1.9.7/bin:$PATH
#export CASSANDRA_HOSTS=127.0.0.1:9042
export ZOOKEEPER_HOST='_address_'
export cassandra_storagedir='/var/lib/cassandra'
export ZOOKEEPER_HOST='somewhere.com'
export cassandra_storagedir='/var/lib/cassandra'
export CASSANDRA_HOSTS='somewhere.com:IN-PORT=xxx.xxx.xxx.xxx:OUT-PORT9042,somewhere.com:IN-PORT=IP-ADDRESS:OUT-PORT,< etc>'
export JAVA_OPTS=$JAVA_OPTS:$CASSANDRA_HOSTS:$cassandra_storagedir:$ZOOKEEPER_HOST

QTDIR=/opt/local/lib/qt3; 
export QTDIR

##
# Your previous /Users/amado/.bash_profile file was backed up as /Users/amado/.bash_profile.macports-saved_2017-12-18_at_13:35:51
##

# MacPorts Installer addition on 2017-12-18_at_13:35:51: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

