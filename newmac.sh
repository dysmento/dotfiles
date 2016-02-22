
#!/bin/bash
if [ "$#" -ne 2 ]; then
   echo "Usage: `basename $0` <github password> <bitbucket password>"
   exit 0
fi

# dock preferences
defaults write com.apple.dock autohide 1
defaults write com.apple.dock largesize 64
defaults write com.apple.dock magnification 1
defaults write com.apple.dock mineffect scale
defaults write com.apple.dock orientation right

# map the caps lock key to control!
# TODO: get the keyboard identifier programmatically
defaults -currentHost write -g com.apple.keyboard.modifiermapping.1452-575-0 -array-add '<dict><key>HIDKeyboardModifierMappingDst</key><integer>2</integer><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer></dict>'

# create an ssh key
[ -f ~/.ssh/id_rsa.pub ] && echo "SSH key created already" \
        || ssh-keygen -q -t rsa -N "" -C "zach.thomas@inin.com" -f "$HOME/.ssh/id_rsa"

# accept XCode license
xcode-select --install

# TODO: get settings.xml for maven
# TODO: get Cisco VPN client at https://indvpn.inin.com
# TODO: install Microsoft Office

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
     wget      \
     awscli    \
     httpie    \
     tmux      \
     boot-clj  \
     jq

brew tap railwaycat/homebrew-emacsmacport
brew install emacs-mac --with-spacemacs-icon
brew linkapps
# git config
# TODO: should we just be using mackup for all the configuration?
git config --global user.name "Zach A. Thomas"
git config --global user.email "zach.thomas@inin.com"
git config --global github.user dysmento
git config --global color.ui true
git config --global push.default simple

# add your new public key to github and bitbucket
http -a "dysmento:$1" https://api.github.com/user/keys title=`hostname` key="`cat ~/.ssh/id_rsa.pub`"
http -a "zathomas:$2" https://api.bitbucket.org/1.0/users/zathomas/ssh-keys accountname=zathomas key="`cat ~/.ssh/id_rsa.pub`" label=`hostname`

# spacemacs, emacs and vim forever!
[ -d ~/.emacs.d ] && echo "emacs configured already" \
        || git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# sdkman
[ -d ~/.sdkman ] && echo "sdkman configured already" \
                         || curl -s get.sdkman.io | bash

# get solarized
[ -f ~/solarized.zip ] && echo "solarized already downloaded" \
                               || curl -O http://ethanschoonover.com/solarized/files/solarized.zip

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
     viscosity          \
     nvalt              \
     java               \
     intellij-idea

# prezto
[ -d ~/.zprezto ] && echo "prezto configured already" || ~/dotfiles-master/prezto.sh

# login items
osascript -e 'tell application "System Events" to make new login item at end with properties {path:"/Users/zach/Applications/LaunchBar.app", name:"LaunchBar", hidden:true}'
osascript -e 'tell application "System Events" to make new login item at end with properties {path:"/Users/zach/Applications/Dropbox.app", name:"Dropbox", hidden:true}'
osascript -e 'tell application "System Events" to make new login item at end with properties {path:"/Users/zach/Applications/Viscosity.app", name:"Viscosity", hidden:true}'
osascript -e 'tell application "System Events" to log out'

