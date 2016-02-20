#!/bin/bash
if [ "$#" -ne 1 ]; then
   echo "Usage: `basename $0` <github password>"
   exit 0
fi

# dock preferences
defaults write com.apple.dock autohide 1
defaults write com.apple.dock largesize 65
defaults write com.apple.dock magnification 1
defaults write com.apple.dock mineffect scale
defaults write com.apple.dock orientation right

# map the caps lock key to control!
defaults -currentHost write -g com.apple.keyboard.modifiermapping.1452-575-0 -array-add '<dict><key>HIDKeyboardModifierMappingDst</key><integer>2</integer><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer></dict>'

# create an ssh key
[ -f ~/.ssh/id_rsa.pub ] && echo "SSH key created already" \
        || ssh-keygen -q -t rsa -N "" -C "zach.thomas@inin.com" -f "$HOME/.ssh/id_rsa"

# accept XCode license
xcode-select --install

# install Homebrew
[ -d /usr/local/Cellar ] && echo "homebrew installed already" \
       || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# all the best shell stuff
brew install   \
     tree      \
     ag        \
     maven     \
     git       \
     zsh       \
     awscli    \
     httpie    \
     jq        \
     emacs-mac

# git config
git config --global user.name "Zach A. Thomas"
git config --global user.email "zach.thomas@inin.com"
git config --global github.user dysmento
git config --global color.ui true
git config --global push.default simple

# add your new public key to github
http -a dysmento:$1 https://api.github.com/user/keys title=`hostname` key="`cat ~/.ssh/id_rsa.pub`"
# spacemacs
[ -d ~/.emacs.d ] && echo "emacs configured already" \
        || git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# add brew support for fonts
brew tap caskroom/fonts

# install casks
brew cask install       \
     font-anonymous-pro \
     launchbar          \
     opera              \
     1password          \
     amazon-music       \
     dropbox            \
     caffeine           \
     java               \
     intellij-idea

# prezto
[ -d ~/.zprezto ] && echo "prezto configured already" || ~/prezto.sh

# login items
LOGIN_ITEMS=[ `osascript -e 'tell application "System Events" to get login item "Launchbar" exists'` ] && \
    [ `osascript -e 'tell application "System Events" to get login item "Dropbox" exists'` ]
if [ LOGIN_ITEMS ]; then
    echo "login items configured already"
else
    osascript -e 'tell application "System Events" to make new login item at end with properties {path:"/Applications/LaunchBar.app", name:"LaunchBar", hidden:true}'
    osascript -e 'tell application "System Events" to make new login item at end with properties {path:"/Applications/Dropbox.app", name:"Dropbox", hidden:true}'
    osascript -e 'tell application "System Events" to log out'
fi

