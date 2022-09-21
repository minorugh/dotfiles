# My dotfiles based on Makefile

## Screenshot
![Debian11 xfce4 desktop](https://live.staticflickr.com/65535/51395292747_c52f2dc3e8_b.jpg) 
![Emacs-27.2](https://live.staticflickr.com/65535/51631946053_b9d848a357_b.jpg)

## Let's build environment with Makefile

This dot file is for Debian Linux. I created it by referring to
[masasam/dotfiles](https://github.com/masasam/dotfiles).

Automation with Make is recommended as it is very easy to customize.

## Debian System Restore is recommended for a clean install from Live-usb
After a clean install, prepare manually before running make.
The guidance is as follows:

```
## =====================================================================
## Manual setting before executing make
## =====================================================================

## 1. Boot from Live-USB to install debian
## Download iso image file from https://www.debian.org/CD/live/
## Create a Live USB stick, https://www.archlinux.site/2018/03/linuxisoubuntulive-usb.html

## 2. Log in as root
## Register username to sudoers
# | gpasswd -a minoru sudo
# | log out
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Log in with ${USER}
## Make home directory English
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git nautilus chromium chromium-l10n
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## 5. Prepare dotfiles
# | mkdir -p ~/src/github.com/minorugh
# | cd src/github.com/minorugh
# | git clone https://github.com/minorugh/dotfiles.git
# | cd dotfiles
# | make install
# | make init

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

* [https://minorugh.github.io/docs/config](https://minorugh.github.io/docs/config) 

----

## update infomeition
* 2021.10.11 Content organization
* 2021.08.26 Update to Debian 11(bullseye)
* 2021.08.26 Update to emacs 27.2
* 2021.02.20 Update to emacs 27.1
* 2021.01.29 Fixed mozc
* 2021.01.28 Fixed so that it can be shared between two Thinkpads
* 2020.11.10 Rebuilding
* 2020.10.27 first commit
