# dotfiles
it's various files I use for awesome configs

## setting up a new mac
So easy! Open a terminal on the new mac, and:

    curl -OL  https://github.com/dysmento/dotfiles/archive/master.zip
    unzip master.zip
    ./dotfiles-master/newmac.sh <github password> <bitbucket password>
    
There are some things that will still be left to do after this runs, since I haven't figured out how to make it automatic:

* install Microsoft Office
* install Cisco AnyConnect Secure Mobility Client

I'm kind of on the fence about whether or not I want to use `mackup` to save configurations

For this to work really well, I'm kind of picturing a folder per software package with a combination of the commands to install it and the additional files (preferences, etc.) it needs.

The base command would be driven by a config file (ini or yaml or edn) that declares which packages to use and any environment necessary to run it. e.g., setting up the SSH public key in bitbucket requires the bitbucket username and password.

The config file would be encrypted and then the whole enchilada could be stored in a public place. What would be really sweet is if you could kick off the whole thing with something like `curl bootstrap.inin.com && ./bootstrap.sh zach.thomas <bootstrap password>` 

If you add in the fact that some packages are going to depend upon others, maybe I really should be using ansible for this!

