
To configure autostart process upon login
(1) (1) add a .sh to /etc/profile.d/


To make a runnable program (bin)
Option 1:
() add a .sh to /etc/profile.d/

logout and login again. It is there
Option 2:
() add to /usr/bin a symbolic link 

That is it!

Or add an export PATH to ~/.profile 

---Examples-
In detail:

(1)
[rosemary@SCUBA bin]$ emacs /etc/profile.d/maven.sh 

----File begin --- 

#!/usr/bin/env sh
export M2_HOME=/opt/apache-maven-3.3.9
export M2=${M2_HOME}/bin
export PATH=${PATH}:${M2}

---File end ----

(Optoin 2) Add the symbolic link

 422  sudo ln -s /opt/apache-ant-1.9.7/bin/ant ant
  492  history | grep ln
  493  sudo ln -s /opt/apache-maven-3.3.9/bin/mvn mvn


[rosemary@SCUBA bin]$ ls -la mvn
lrwxrwxrwx 1 root root 31 Dec 31 21:55 mvn -> /opt/apache-maven-3.3.9/bin/mvn
[rosemary@SCUBA bin]$ 




~/.bash_profile is only sourced by bash when started in interactive login mode. That is typically only when you login at the console (Ctrl+Alt+F1..F6), or connecting via ssh.

When you log in graphically, ~/.profile will be specifically sourced by the script that launches gnome-session (or whichever desktop environment you're using). So ~/.bash_profile is not sourced at all when you log in graphically.

When you open a terminal, the terminal starts bash in (non-login) interactive mode, which means it will source ~/.bashrc.

The right place for you to put these environment variables is in ~/.profile, and the effect should be apparent next time you log in.

Sourcing ~/.bash_profile from ~/.bashrc is the wrong solution. It's supposed to be the other way around; ~/.bash_profile should source ~/.bashrc.

See DotFiles for a more thorough explanation, including some history of why it is like it is.

(On a side note, when installing openjdk via apt, symlinks should be set up by the package, so that you don't really need to set JAVA_HOME or change PATH)

JakeGould
16717
answered Apr 11 '12 at 16:41

geirha
22.3k84753
3	 	
I've found that when opening a Terminal from the sidebar in Ubuntu 12 the ~/.profile file is not loaded. – jcollum Mar 24 '13 at 17:20
1	 	
@jcollum That's good. .profile should only be sourced when you log in. – geirha Mar 30 '13 at 6:59
2	 	
oh, opening a terminal is not the same as logging in... I was thinking logging in to the terminal. – jcollum Mar 31 '13 at 16:53
1	 	
I know, that's why I added my additional code there. What I wanted to do was to configure options for the terminal (e.g. the prompt), but these were not loaded if I set them on .profile, even after a full restart. – Juan A. Navarro Jul 4 '13 at 20:02
1	 	
@JuanA.Navarro Ah, you mean PS1 and PROMPT_COMMAND etc? Those are not environment variables, and they do indeed belong in ~/.bashrc as they only make sense for interactive shells. – geirha Jul 4 '13 at 20:30




A shell is the generic name for any program that gives you a text-interface to interact with the computer. You type a command and the output is shown on screen.

Many shells have scripting abilities: Put multiple commands in a script and the shell executes them as if they were typed from the keyboard. Most shells offer additional programming constructs that extend the scripting feature into a programming language.

On most Unix/Linux systems multiple shells are available: bash, csh, ksh, sh, tcsh, zsh just to name a few. They differ in the various options they give the user to manipulate the commands and in the complexity and capabilities of the scripting language.

Interactive: As the term implies: Interactive means that the commands are run with user-interaction from keyboard. E.g. the shell can prompt the user to enter input.

Non-interactive: the shell is probably run from an automated process so it can't assume if can request input or that someone will see the output. E.g Maybe it is best to write output to a log-file.

Login: Means that the shell is run as part of the login of the user to the system. Typically used to do any configuration that a user needs/wants to establish his work-environment.

Non-login: Any other shell run by the user after logging on, or which is run by any automated process which is not coupled to a logged in user.
