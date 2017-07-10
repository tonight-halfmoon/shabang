
  #local jobnum=`jobs %% |awk '{print $1}'`
                 #echo "job $jobnum" >&2
                 #eval 'kill -9 $!' >&2 # &> /dev/null
                 #eval 'kill ${jobnum:1:1}' >&2
