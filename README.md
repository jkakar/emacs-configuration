This is my emacs configuration.  I use it with Emacs 24 on Mac OS X Yosemite.
With some minor tweaks it will run on Ubuntu too.

You need to link the `~/.emacs` and `~/.emacs.d` to the files in the branch:

    cd ~/
    ln -s ~/src/jkakar/emacs-configuration/emacs .emacs
    ln -s ~/src/jkakar/emacs-configuration/emacs.d .emacs.d

The `emacs.d` directory contains `local` and `plugins` subdirectories.  The
`local` directory contains local configuration, organized into files by type.
The `plugins` directory contains emacs modules.
