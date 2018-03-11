/home/piotr/.fehbg &
xrdb -load ~/.Xresources &
xrandr --dpi 110 &
setxkbmap -layout pl &
setxkbmap -option "ctrl:nocaps" &
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh) && export GNOME_KEYRING CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK &
dunst &
compton -b --config .config/compton/compton.conf &
redshift &
parcellite &
/home/piotr/bin/touchpad.sh &
i3
