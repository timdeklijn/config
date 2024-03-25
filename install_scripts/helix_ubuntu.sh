#!/bin/bash

curl -LO https://github.com/helix-editor/helix/releases/download/23.10/helix-23.10-x86_64.AppImage
chmod +x helix-*.AppImage # change permission for executable mode
sudo mv helix-*.AppImage ~/bin/heliX
