# Keymap notes

Install:

``` sh
brew install qmk/qmk/qmk
```

Then setup `qmk`:

``` sh
qmk setup
```

Go to the folder containing the qmk firmware and create a new folder
in the keymaps folder at the right keyboard: `splitkb/aurora/sweep/`.
Edit the file to have the correct mapping and run:

``` sh
qmk compile -kb splitkb/aurora/sweep -km tim -e CONVERT_TO=promicro_rp2040
```

This will create a `.uf2` file in the root of the `qmk_firmware`
folder. Plug in one half of the keyboard and press the reset button
twice. Open a finder window and drag the `.uf2` file to the disk that
just appeared. Repeat and the new mapping should now be flashed onto
the keyboard.

## TODO

- Bash script to automatically compile and open some finder windows
