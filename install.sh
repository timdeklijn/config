# Install Script

# Create symlink to init.vim
echo "-- Replacing nvim config file"
rm ~/.vimrc
ln -s $(pwd)/vim.vim ~/.config/nvim


# Create symlink to .zshrc
echo "-- Replacing zshrc config file"
rm ~/.zshrc
ln -s $(pwd)/zshrc ~/.zshrc

# Create symlink to .tmux.conf
echo "-- Replacing tmux config file"
rm ~/.tmux.conf
ln -s $(pwd)/tmux.conf ~/.tmux.conf
