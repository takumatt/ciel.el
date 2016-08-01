## Introduction

An emacs command that is clone of "ci" in vim.  
You can use ci", ci', ci(, ci{ and ciw entering `Ctrl-c, i`.   
Also you can copy them with `Ctrl-c, o` instead of `Ctrl-c, i`.  
This is standalone package and you can probably use any mode.  

![circleanimationmuvie](https://raw.githubusercontent.com/cs14095/cs14095.github.io/master/ci-el.gif) 

**Now at work! I highly recommend updating every week!**  
I decided to remove cit on master branch, because it's too heavy.  
Other command is still available, but some code is broken.  
I will fix in summer vacation...  




## Installation

Download ci.el somewhere.  
For example:


	cd ~/.emacs.d/elisp/
	git clone https://github.com/cs14095/ciel.el


Then add the following in your .emacs file:


	(setq load-path (cons "~/.emacs.d/elisp/ciel.el" load-path))
	(require 'ciel)


## Usage

Press `Ctrl-c, i` or `Ctrl-c, o` and enter available character.  
Watch example or vim usage.  


## Example

	Ctrl-c, i, w => kill a word  
	Ctrl-c, i, [<>] => kill inside <>  
	Ctrl-c, i, ' => kill inside ''
	Ctrl-c, i, " => kill inside ""  
	Ctrl-c, i, [()] => kill inside ()  
	Ctrl-c, i, [{}] => kill inside {}  
	
	Ctrl-c, o, w => copy a word  
	Ctrl-c, o, [<>] => copy inside <>  
	Ctrl-c, o, ' => copy inside ''
	Ctrl-c, o, " => copy inside ""  
	Ctrl-c, o, [()] => copy inside ()  
	Ctrl-c, o, [{}] => copy inside {}  

you can also kill nested parentheses as you can see.

![circleanimationmuvie](https://raw.githubusercontent.com/cs14095/cs14095.github.io/master/ci-el.gif) 


## Features
 - More flexible  
   You will be able to kill inside different characters like ")" and "}".  
   

## Conclusion

Almost commands that I know are ready.  
I will continue to add commands and fix bugs, so please give me any comments.


## License
MIT
