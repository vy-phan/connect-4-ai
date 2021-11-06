# connect-4-ai
# CS 580 - GMU - Final Project

;;;; CONNECT FOUR PLAYER
;;;;
;;;; RUNNING THE CODE
;;;;
;;;; (load "connectfour")
;;;; (compile-file "connectfour.lisp")
;;;; (load "connectfour")
;;;; (load "sean-luke")
;;;; (compile-file "sean-luke.lisp")
;;;; (load "sean-luke")
;;;;
;;;; If you want to just do a quick human-human game, you
;;;; don't need to compile and load the player.lisp file, just the connectfour file.
;;;;
;;;; You can run a simple human-against-human example like this:
;;;;
;;;; (play-human-human-game)
;;;;
;;;; You can run a simple human-against-computer example like this (replace sean-luke
;;;; with your own package name in this example of course)
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move t)  
;;;;
;;;; ...or if you wish the computer to go first,
;;;;
;;;; (play-human-computer-game #'sean-luke:make-computer-move nil)
;;;;
;;;; You can play a head-to-head game against two computer players (loading both of course)
;;;; like this:
;;;;
;;;; (play-game #'sean-luke:make-computer-move #'keith-sullivan:make-computer-move)
;;;;
;;;; Note that for all three of these functions (play-human-human-game, play-human-computer-game,
;;;; play-game) there are lots of optional keywords to change the width o the board, the height,
;;;; the number in a row that must be achieved, the search depth, whether boards are printed, etc.
