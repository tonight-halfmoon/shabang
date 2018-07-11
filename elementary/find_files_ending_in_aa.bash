for file in /home/rosemary/programming/unix/{,sub/}*aa

do
	if [ -x "$file" ]
	then
	     echo $file
	fi
done

echo 'I am done'
