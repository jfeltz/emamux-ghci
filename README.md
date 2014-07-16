emamux-ghci
===========
emamux-ghci is a small library wrapping [emamux](http://www.github.com/syohex/emacs-emamux) in-order to
stream line usage of a [tmux](http://tmux.sourceforge.net) based GHCI session in [Emacs](http://www.gnu.org/s/emacs/). The primary features of emamux-ghci are file and buffer loading, plus management of include paths and language extension settings for the current loaded modules.
<p align="center">
  <href a="http://www.github.com/jfeltz/ememamux-ghci/blob/master/example.png?raw=true"> 
    <img src="http://www.github.com/jfeltz/emamux-ghci/blob/master/example.png?raw=true" style="border-style: none" width="600px"/>
  </a>
</p>

Why?
====

  emamux-ghci is not a substitute for [inferior-haskell](https://github.com/haskell/haskell-mode/wiki/Inferior-Haskell-Mode) (which definitely has more features), however, one may find it useful for the following reasons:

* Shared Use of GHCI Between Editors 

    Allows for the shared use of a single GHCI session by multiple editors (E.g. Emacs, Vim, Eclipse)

* Dodge Configuration Hell, Simplify the GHCI Presentation Problem

    This is useful as a hedge against a potential maintenance gap/shortfall for [inferior-haskell](https://github.com/haskell/haskell-mode/wiki/Inferior-Haskell-Mode) or in the event of unresolvable config issues with inferior-haskell. There is also still a need to use GHCI directly for its features such as color handling and completion, and that is somewhat of a moving target with respect to the rest of the Emacs ecosystem.

Installation
============
Requirements:
* Emacs (clearly), tested with 24.3.1
* tmux, tested with 1.3
* emamux, tested with emamux-20140701
* GHC (and by implication, GHCI), tested with 7.6.3 and 7.8.2

Procedure:
Just download <b>emamux-ghci.el</b> and drop it into
an appropriate library load path, e.g. for /path/to/lib/:

    (add-to-list 'load-path "~/path/to/lib/")
    (require 'emamux-ghci)

Setup & Tips 
============

  Starting tmux 

     $ tmux new-session -n ghci -s haskell 'cabal repl'

  Tell emamux-ghci where to find the address to the tmux session:

    ; Note, "haskell:ghci" is the default
    (setq emamux-ghci:tmux-address "haskell:ghci")

  Add haskell module (or project) specific settings:

    (setq emamux-ghci:includes '("src" "tests"))
    (setq emamux-ghci:exts '("UnicodeSyntax" "GADTs"))

  Note, a <i>emamux-ghci:proj-sync</i> is performed on
  emamux-ghci:proj-load-[buffer|file] if project settings have been
  changed. However, this can be done manually:

    M-x emamux-ghci:proj-sync

  Example keybindings (assuming use with haskell-mode): 

    (define-key haskell-mode-map [f2] 'emamux-ghci:proj-load-buffer)
