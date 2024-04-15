#!/bin/bash
#
# Tim de Klijn, 2024
#
# Install script for sublime packages. Keep the sublime install reproducable
# between machines.

# if first command line arg is `new` run "install_package_control"
if [ "$1" = "new" ]; then
	subl --command "install_package_control"
fi

# An array with packages that will be installed.
declare -a packages=( \
	"LSP" \
	"LSP-copilot" \
	"LSP-gopls" \
	"LSP-pyright" \
	"SublimeLinter-pylint" \
	"SublimeLinter-mypy" \
	"Terminus" \
)

# loop over packages and install them one by one
for package in "${packages[@]}"; do
	echo "Installing package $package"
	subl --command "advanced_install_package {\"packages\": \"$package\"}"
done
