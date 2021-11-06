# CONNECT FOUR GAME
#### A simple, copycat version of the classic game ( in the alien language LISP ) where humans can play against each other or against *The bot* and win by connecting four tiles vertically, horizontally, or diagonally. Or, the bot can possibly wipe the floor with you. No hard feelings... at least from the bot.

 - Recommended Emacs + SLIME + sbcl
 - Run the following commands to compile and load the files in correct order
   ```
   (load "connectfour")
   (compile-file "connectfour.lisp")
   (load "connectfour")
   (load "bot")
   (compile-file "bot.lisp")
   (load "bot")
   ```
    
 - If you want to just do a quick human-human game, you don't need to compile and load the bot.lisp file, just the connectfour.lisp file. 
   You can run a simple human-against-human example like this:
   ```
   (play-human-human-game)
   ```
    
 - You can run a simple human-against-computer example like this:
   ```
   (play-human-computer-game #'bot:make-computer-move t)
   ```
   ...or if you wish the computer to go first:
   ```
   (play-human-computer-game #'bot:make-computer-move nil)
   ```
    
NOTE
 - There are lots of optional keywords to change the width and height of the board, the number in a row that must be achieved, 
   the search depth, whether boards are printed, etc.
