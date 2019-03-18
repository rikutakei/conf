# My configuration files for everything

This repo contains all of the configuration files I use for most of my desktop environment.

# How do I install...

## Vim configs

The vim config is probably the most annoying to install (and probably the only one worth mentioning how to install), because you need to use different `.vimrc` file depending on the vim version.
This is due to the use of a different vim plugin manager: version < 8.0 uses [`NeoBundle`](https://github.com/Shougo/neobundle.vim) and version >= 8.0 uses [`Dein.vim`](https://github.com/Shougo/dein.vim).

So, please check your vim version first, and run stuff according to the right version.

### Version <= 8.0 (and above > 7.2)

Run:

```bash

# Setup the directory structure to make Ultisnip and persistent Undos work:
bash ./setupdir.sh

# Either copy or symlink the vim7.4/.vimrc to your home directory:
cp path/to/conf/vim7.4/.vimrc ~/.vimrc

# Install NeoBundle first:
sh ./install_neobundle.sh

# Install the plugins by running the command below, or open vim and run ":NeoBundleInstall":
vim +NeoBundleInstall +qall

```

### Version > 8.0

Run:

```bash

# Setup the directory structure to make Ultisnip and persistent Undos work (hopefully):
bash ./setupdir.sh

# Either copy or symlink the vim7.4/.vimrc to your home directory:
cp path/to/conf/vim8/.vimrc ~/.vimrc

# Install NeoBundle first:
sh ./install_dein.sh ~/.vim/dein

```

Open vim and run the following in command mode:

```bash
:call dein#install()
```

## Xmonad config

I have an Xmonad config file for use in my linux desktop, where I use a hybrid of KDE and xmonad desktop environment/windows manager.

Just briefly, xmonad is essentially a *tmux* for normal windows like terminal, web browser, file browser, etc.

Move or copy `xmonad.hs` file to `~/.xmonad` and compile it:

```bash

cp path/to/conf/xmonad.hs ~/.xmonad/xmonad.hs

xmonad --recompile

```

Either reboot or logout and log back in for the changes to reflect.

## Other config files

Other config files for `tmux`, `bash`, `inputrc` or whatever config files you can find in this directory, just copy or symlink it to your home directory.

