#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11R6/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11R6/lib/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f $userresources ]; then
    xrdb -merge $userresources
fi

if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi

# layout de teclado br_abnt2
/usr/X11R6/bin/setxkbmap -model abnt2 -layout br -variant abnt2

# adjust backlight brightness using RandR extension
/usr/X11R6/bin/xbacklight -set 50

# Start the window manager:
exec /usr/X11R6/bin/fluxbox
