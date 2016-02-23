#!/bin/bash
xcode-select --install
sudo easy_install_pip
sudo pip install ansible
mkdir ~/dev
git clone https://github.com/dysmento/starter.git ~/dev/starter
cd ~/dev/starter
ansible-galaxy install -r requirements.yml && ansible-playbook --ask-vault-pass desktop.yml
osascript -e 'tell application "System Events" to log out'
