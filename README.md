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
## 1. Boot from USB to install Debian latest
# Create installation USB from netinst iso image. Use Rufs.exe on Windows
# rufs https://rufus.ie/ja/

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a ${USER} sudo
# | sudo nano /etc/sudoers
# Add and edit correction as follows
# ----------------------------------
# # User privilege specification
# root    ALL=(ALL:ALL) ALL
# minoru ALL=(ALL:ALL) NOPASSWD:ALL
# # Allow members of group sudo to execute any command
# %sudo   ALL=(ALL:ALL) NOPASSWD:ALL
# ----------------------------------

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk ## Not needed for debian12 or later
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y make git nautilus

## 4. Install dropbox & setting
# Before installing, configure the Synapyic repository & Check existence of package
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and initial settings

## 5. Import GPG private_key
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/gpgimport.git
# | cd gpgimport
# | make gpg

## 6. Clone dotfiles from GitHub
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/dotfiles.git
# | cd dotfiles
# | git-crypt unlock
# | make all

## 7. Change shell to zsh
# | chsh -s /usr/bin/zsh

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
