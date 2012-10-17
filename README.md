This is my emacs configuration.  The first step is to install package
dependencies:

    ./install-packages.sh

Once that's done, you'll need to link the `~/.emacs` and `~/.emacs.d`
to the files in the branch:

    cd ~/
    ln -s ~/src/jkakar/emacs-configuration/emacs .emacs
    ln -s ~/src/jkakar/emacs-configuration/emacs.d .emacs.d

The `emacs.d` directory contains `local` and `plugins` subdirectories.
The `local` directory contains local configuration, organized into
files by type.  The `plugins` directory contains unpackaged emacs
modules.
