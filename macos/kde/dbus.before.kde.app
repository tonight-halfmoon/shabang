launchctl load -w /Library/LaunchAgents/org.freedesktop.dbus-session.plist


Don't forget that dbus needs to be started as the local user (not with sudo)
    before any KDE programs will launch.
    To start it run the following command:
     launchctl load -w /Library/LaunchAgents/org.freedesktop.dbus-session.plist