#!/bin/bash

e_noArgs=59
e_notFound=60
e_notDirectory=61
e_unable2Change2BaseDir=62
e_unable2StartFrontEnd=63

d_startedin=`pwd`
echo "d_startedin= $d_startedin" >&2

c_pullFirst=0
c_defaultGradleTasks="clean build"

l_logfile=$d_startedin/script.log
log_errors=$d_startedin/errors.log

p_bootproject=unknown

if [ $# -eq 0 ]
then
	echo "Usage: `basename $0` <base directory>" >&2
	echo "Incorrect usage" 2>>$log_errors
	exit $e_noArgs
fi

cleanLogs () {
	: > $l_logfile
	: > $log_errors
}

checkProvidedDir () {
	#if [ ! -f "$basedir" ]
	#then
	#	echo "Directory $basedir not found!" >&2
	#	exit $e_notFound
	#fi
	
	basedir=$1
	if [ ! -d "$basedir" ]
	then		
		echo "$basedir not a directory" >&2
		echo "$basedir not a directory" 2>>$log_errors
		exit $e_notDirectory
	fi	
}

#pushed $dir1
#popd

change2BaseDir () {
	pushd $basedir
	# Set basedire as absolute path
	basedir=`pwd`
	echo "Base directory = $basedir" 1>>$l_logfile
	if [ ! "$basedir" == `pwd` ]
	then
		echo "Not in target Base directory" >&2
		echo "Not in target Base directory" 2>>$l_logfile
		exit $e_unable2Change2BaseDir
	fi
	echo "In target Base directory" >&2
	echo "Current directory = `pwd`" >&2
	echo "Base directory= $basedir" >&2
	echo "Current directory = `pwd`" 1>>$l_logfile
}

assignBootProject () {
	if [ ! -z "$1" ]
	then	
		p_bootproject=$basedir/$1
		echo "Boot project= $p_bootproject" >&2
		echo "Boot project= $p_bootproject" 1>>$l_logfile
	else
		echo "Boot project is $p_bootproject" >&2
		echo "Boot project is $p_bootproject" 1>>$l_logfile
	fi
}

shouldUpdateWC () {
	if [ ! -z "$1" ] && [ "$1" -eq 1 ]
	then
		echo "Pull first is set to true" >&2
		echo "Pull first is set to true" 1>>$l_logfile
		c_pullFirst=$1
	fi
}

initialiseVariables () {
	numdirs=0
	num_gradleProjects=0
	echo "Variables initialised" 1>>$l_logfile
}

goinside () {
	echo "Visiting each project... " >&2
	
	for dir in `echo *`
	do	
		if [ -d "$dir" ]
		then	
			echo "+--$dir" >&2 #`ls -l $dir | sed 's/^.*'$dir' //'`
			echo "+--$dir" 1>>$l_logfile
			numdirs=`expr $numdirs + 1`
			if can_cd 
			then 
				execute_gradle_tasks
				cd ..
				echo "Changed to Base directory" >&2
				echo "Changed to Base directory" 1>>$l_logfile
			fi
		fi		
	done
}

can_cd() 
{
	if ! cd "$dir"
	then
		echo "Not able to change into directory $dir" >&2
		echo "Not able to change into directory $dir" 2>>$log_errors
		return 1
	else
		return 0
	fi

}

is_gradleProject() {
	if [ ! -f "build.gradle" ] 
	then
		echo "Not a Gradle project" >&2
		echo "Not a Gradle project" 2>>$log_errors
		return 1
	else
		#ls build.gradle 1>>$l_logfile
		return 0
	fi	
}
	
updateWC () {
	if [ "$c_pullFirst" -eq 1 ]
	then
		echo "Let's update your working copy" >&2
		echo "Let's update your working copy" 1>>$l_logfile
		git pull 1>>$l_logfile 2>>$log_errors
	fi	
}

execute_gradle_tasks () {
	#if cd "$dir" && [ -f "build.gradle" ] 
	#if cd "$dir"  && : () { [ -f "build.gradle" ]  ;  }; :
	if is_gradleProject
	then
		num_gradleProjects=`expr $num_gradleProjects + 1`
		echo "Changed into project $dir" >&2
		echo "Changed into project $dir" 1>>$l_logfile
		updateWC
		echo "Let's execute gradle $defaultGradleTasks" >&2
		echo "Let's execute gradle $defaultGradleTasks" 1>>$l_logfile
		#gradle -q check
		gradle $defauleGradleTasks 1>>$l_logfile 2>>$log_errors
	fi	
}

stopGradleDaemons () {
	echo "Stopping all Gradle's Daemon(s)" >&2 1>>$l_logfile
	#gradle -v --stop
	gradle --stop 1>>$l_logfile
}

runProjects () {
	echo "Running target projects .. " >&2
	if ! assurePWDisBaseDir 
	then
		echo "Won't Run Projects" >&2
		return 0
	fi
	echo "Changing to boot project " >&2
	cd $p_bootproject >&2
	echo "Current directory= `pwd`" >&2
	local log_registry=$PWD/registry.log
	local log_cardesire=$PWD/cardesire.log
	local log_payment=$PWD/payment.log
	local log_dealer=$PWD/dealer.log
 	: > $log_registry
 	: > $log_cardesire
 	: > $log_payment
 	: > $log_dealer	

	echo "Registry" >&2 
	gradle -Pmsname=registry runMS & >&$log_registry #2>&$log_registry
	sleep 5
	echo "Cardesire" >&2
	gradle -Pmsname=cardesire runMS & >&$log_cardesire # 2>&$log_cardesire
	sleep 2
	echo "Dealer" >&2
	gradle -Pmsname=dealer runMS & >&$log_dealer # 2>&$log_dealer
	sleep 2
	echo "Payment" >&2
	gradle -Pmsname=payment runMS & >&$log_payment # 2>&$log_payment
	sleep 2	
}

startDockerContainers () {
	exec $d_startedin/docker_daemon_start.sh & >&2
	while ! docker system info &>/dev/null
	do
		(( i++ == 0 )) && printf %s ' -- Docker Daemon is starting ....' || printf '.'
		sleep 1
	done
	(( i )) && printf '\n'
	echo "Docker is ready" >&2
	docker start some-rabbit mongodb ecomgw-postgres & 1>>$l_logfile 2>>$log_errors
 	#exec $d_startedin/docker_containers_start.sh & >&2	
}

startFrontEnd () {
	echo "starting front end..." >&2
	cd $basedir/$1
	echo "pwd=`pwd`" >&2 
	local log_npm_start=$PWD/npm_start.log
	: > $log_npm_start
	npm start & 1>>$log_npm_start 2>>$log_npm_start	
	popd
}

assurePWDisBaseDir () {
	echo "Current directory= `pwd`" >&2
	if [ `pwd` != "$basedir" ]
	then
		echo "Changing to Base directory" >&2
		cd $basedir 1>>$l_logfile
	fi
	echo "After changed to Base directory..." >&2
	if [ `pwd` != "$basedir"  ]
	then
		echo "Unable to change to Base directory" >&2
		echo "Unable to change to Base directory" 2>>$log_errors
		return 1
	fi
	echo "Current directory= `pwd`" >&2
	echo "Current directory= `pwd`" 1>>$l_logfile
	return 0
}

printStats () {
	echo "Total projects visited = $numdirs" >&2 #1>>$l_logfile
	echo "Total gradle projects visited = $num_gradleProjects" >&2 #1>>$l_logfile
}
  
#checkArgs
cleanLogs
checkProvidedDir $1
change2BaseDir
assignBootProject $3
initialiseVariables
shouldUpdateWC $2
stopGradleDaemons
goinside 0
startDockerContainers
runProjects 0 
startFrontEnd $4
printStats 0

echo 
echo "Job done." >&2
echo "Job done." 1>>$l_logfile

exit 0
