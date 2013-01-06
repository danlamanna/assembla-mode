assembla-mode
===============

A major mode allowing for interaction with Assembla.
This allows for the most basic of interactions, viewing spaces, viewing tickets and ticket comments, and adding ticket comments.

Installation
----
**Note: assembla-mode has a strict dependency on assembla-lib, as such, it's a submodule.**

1) `git clone https://github.com/danlamanna/assembla-mode.git`    
2) `git submodule init`    
3) `git submodule update`    
4) Configure assembla-lib if not already done, by following instructions [there][1].    

Usage
----
This isn't nearly as fast as desired without caching, so enabling `asl/cache-enabled` in `M-x customize` should take care of that.
    
**Note: First load may be considerably slow while caching all spaces/users. Without caching, if you're a part of many
spaces with many users, this may not be performant enough to work with.**

    M-x assembla
From here you should see a list of the spaces you're involved with, hitting `C-c f` will call `asm/goto-thing-at-point`
which will show a list of all tickets in that space. You can continue the trend of `C-c f` to view the tickets at a higher level of detail.

Pressing `C-c d` will call `asm/prev-buffer` which will bring you back to the last buffer you were viewing.

When viewing a single ticket, pressing `C-c c` will open a new empty buffer for a ticket comment, after filling out
the buffer with your comment, and pressing `C-c C-c` - your comment will be POSTed to Assembla and your buffer
should refresh showing you the new comment.

Contributing
----
Pull Requests are greatly encouraged!

If you're looking for work, see the "Todo" section in the top block of comments, or look throughout the
code for TODOs in function blocks.


[1]: https://github.com/danlamanna/assembla-lib
