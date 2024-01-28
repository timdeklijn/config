#!/bin/bash

repos=(
    "https://github.com/minad/vertico.git"
    "https://github.com/emacs-compat/compat.git"
    "https://github.com/minad/marginalia.git"
    "https://github.com/oantolin/orderless.git"
    "https://github.com/copilot-emacs/copilot.el.git"
    "https://github.com/magnars/dash.el.git"
    "https://github.com/magnars/s.el.git"
    "https://github.com/editorconfig/editorconfig-emacs.git"
    "https://codeberg.org/akib/emacs-eat.git"
    "https://github.com/bbatsov/projectile.git"
    "https://github.com/justbur/emacs-which-key.git"
    "https://github.com/magit/magit.git"
    "https://github.com/magit/with-editor.git"
    "https://github.com/doomemacs/themes.git"
    "https://github.com/nashamri/spacemacs-theme.git"
    "https://github.com/jrblevin/markdown-mode.git"
    "https://github.com/wbolster/emacs-direnv.git"
    "https://github.com/integral-dw/org-superstar-mode.git"
    "https://github.com/json-emacs/json-mode.git"
    "https://github.com/yoshiki/yaml-mode.git"
    "https://github.com/Sterlingg/json-snatcher.git"
    "https://github.com/purcell/exec-path-from-shell.git"
    "https://github.com/tarsius/minions.git"
    "https://github.com/minad/corfu.git"
    "https://github.com/minad/cape.git"
    "https://github.com/jorgenschaefer/emacs-buttercup.git"
    "https://github.com/magit/libegit2.git"
)

# Move into the packages directory
pushd ~/.config/emacs/packages
for r in ${repos[@]}; do
    git clone $r
done
popd

