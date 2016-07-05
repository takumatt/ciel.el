# ci.el
An emacs command that is clone of "ci" in vim.

# Installation
Download ci.el somewhere
For example:

~~~~
cd ~/.emacs.d/elisp/
git clone --recursive https://github.com/cs14095/ci.el
~~~~

Then add the followint in your .emacs file:

~~~~
(setq load-path (cons "~/.emacs.d/elisp/ci.el" load-path))
(require 'ci)
~~~~

# Usage
Press Ctrl-c, i then enter available character.
Watch example.

# Example
Ctrl-c, i, w => kill a word
Ctrl-c, i, t => kill inside of <>
Ctrl-c, i, " => kill inside of ""
Ctrl-c, i, ( => kill inside of ()
Ctrl-c, i, { => kill inside of {}

you can also kill large area.
(I'll post gif.)
