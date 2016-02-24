#!/bin/bash
xcode-select --install
sudo easy_install pip
sudo pip install ansible
mkdir ~/dev
git clone https://github.com/dysmento/starter.git ~/dev/starter
cd ~/dev/starter
ansible-galaxy install -r requirements.yml && ansible-playbook --ask-vault-pass --ask-become-pass purecloud.yml
