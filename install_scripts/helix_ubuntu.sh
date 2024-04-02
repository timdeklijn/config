#!/bin/bash

curl -LO https://github.com/helix-editor/helix/releases/download/24.03/helix-24.03-x86_64.AppImage
chmod +x helix-*.AppImage # change permission for executable mode
sudo mv helix-*.AppImage ~/bin/helix
