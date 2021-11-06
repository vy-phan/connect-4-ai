# CONNECT FOUR PLAYER


TO RUN THIS CODE:
 - Recommended Emacs + SLIME + sbcl
 - Run the following commands to compile and load the files in correct order
      (load "connectfour")
      (compile-file "connectfour.lisp")
      (load "connectfour")
      (load "bot")
      (compile-file "bot.lisp")
      (load "bot")
    
 - If you want to just do a quick human-human game, you don't need to compile and load the player.lisp file, just the connectfour file. 
   You can run a simple human-against-human example like this:
      (play-human-human-game)
    
 - You can run a simple human-against-computer example like this:
      (play-human-computer-game #'bot:make-computer-move t)     
   ...or if you wish the computer to go first:   
      (play-human-computer-game #'bot:make-computer-move nil)
    
NOTE
 - There are lots of optional keywords to change the width o the board, the height the number in a row that must be achieved, 
   the search depth, whether boards are printed, etc.
