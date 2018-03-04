/home/piotr/.fehbg &
xrdb -load ~/.Xresources &
xrandr --dpi 110 &
setxkbmap -layout pl &
setxkbmap -option "ctrl:nocaps" &
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh) && export GNOME_KEYRING CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK &
xfce4-panel --disable-wm-check &
nm-applet &
sleep 2 && dropbox start &
dunst &
compton -b --config .config/compton/compton.conf &
/usr/lib/deja-dup/deja-dup-monitor &
parcellite &
/home/piotr/bin/touchpad.sh &
i3
