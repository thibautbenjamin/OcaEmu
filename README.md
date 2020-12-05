# OcaEmu
An OCaml-based emulator for the Space Invaders arcade machine that I have coded following the tutorial at [emulator101.com](www.emulator101.com).

## Installation
You need to install OCaml and the opam package manager which is used for installing all the OCaml libraries. I have developed this project with OCaml 4.11.1, but I hope it to be retrocompatible with older 4.0x versions, until 4.03. The graphics and events part are done in SDL using Tsdl, so you also need to install the SDL library on your system. 

Install the OCaml libraries needed to compile this software by running
`opam install unix stdint tsdl`

Then clone this repository and compile the software by simply running `make`, this will produce an executable called emu

## Usage
This repository does not contain the Space Invaders ROM, which you have to either dump or find on the internet. The ROM is found in a file called invaders.zip, which while unzipped contains 4 sub files. You need to concatenate all these files by running

    cat invaders.h > invaders
    cat invaders.g >> invaders
    cat invaders.f >> invaders
    cat invaders.e >> invaders

and then place the resulting file called `invaders` in the project directory. You can now start the emulator by running `./emu invaders` from within the directory.
    
