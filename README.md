# Iridium

A version of [xmonad](http://xmonad.org/) which abstracts away X11 and
is written in and configured by Idris, rather than Haskell.

## Usage

To cycle window focus, use Cmd-Option-j (`⌘-⌥-j`) and Cmd-Option-k (`⌘-⌥-k`).
To cycle window position, use Cmd-Option-Shift-j (`⌘-⌥-⇧-j`) and Cmd-Option-Shift-k (`⌘-⌥-⇧-k`).
To cycle through window layout strategies, use Cmd-Option-Spacebar.

## Installation

To build iridium, you'll need to be on a Mac, with XCode installed, and you'll
need the idris language. Instructions on installing idris can be found
[here](http://www.idris-lang.org/download/), though in most cases this will
amount to:

    cabal update
    cabal install idris

If you do not yet have cabal and the rest of the haskell platform, this project
may not be for you.

After installing idris, simply run `make` in the top-level project directory.
