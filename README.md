# My dotfiles on Makefile

## Screenshot
![Debian11 xfce4 desktop](https://live.staticflickr.com/65535/51395292747_c52f2dc3e8_b.jpg) 
![Emacs-29.1](https://live.staticflickr.com/65535/53032684552_3f0767459c_b.jpg)

## Let's build environment with Makefile

This dot file is for Debian Linux. I created it by referring to
[masasam/dotfiles](https://github.com/masasam/dotfiles).

Automation with Make is recommended as it is very easy to customize.

## First, do a clean install of debian from the install USB
After a clean install, prepare manually before running make.
The guidance is as follows:

```
## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to netinstall Debian latest
# Create installation USB from netinst iso image, use Rufs.exe on Windows
# Download firmware from https://bre.is/f2LBmD3t
# Unzip firmware.zip, then paste to firmware directory of install USB

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a ${USER} sudo
# | logout
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git make nautilus
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## 5. Restore dotfiles
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone https://github.com/minorugh/dotfiles.git
# | cd dotfiles
# | make all

## =====================================================================
## Run make from here
## =====================================================================

After this, refer to makefile
```

## Restore procedure with makefile
How to restore with makefile,Please refer to 
[masasam/dotfiles](https://github.com/masasam/dotfiles). 

## My GNU Emacs configuration 
Detailed explanation is written at the following URL.

* [https://minorugh.github.io/.emacs.d](https://minorugh.github.io/.emacs.d/) 

----

## update infomeition
* 2021.11.01 Remote repository also on xserver (simultaneous Push)
* 2021.10.11 Content organization
* 2021.08.26 Update to Debian 11(bullseye)
* 2021.08.26 Update to emacs 27.2
* 2021.02.20 Update to emacs 27.1
* 2021.01.29 Fixed mozc
* 2021.01.28 Fixed so that it can be shared between two Thinkpads
* 2020.11.10 Rebuilding
* 2020.10.27 first commit
