# Installation Instruction

## NeoVIm

### Ubuntu

Install neovim:

```sh
sudo apt install neovim
```

Install vim-plug

```sh
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

For CoC language server plugin install node.js:

```sh
curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs
```

## Kitty

Install Nord theme for kitty

```sh
curl -o ~/.config/kitty/nord.conf https://raw.githubusercontent.com/connorholyday/nord-kitty/master/nord.conf
```

And include in `# ~/.config/kitty/kitty.conf`:

```
include nord.conf
```
