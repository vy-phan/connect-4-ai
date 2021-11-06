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
;;;;

(defpackage :vy-phan
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :vy-phan)

(defun alpha-beta (game current-depth max-depth
		           is-maxs-turn-p expand terminal-p evaluate
		           alpha beta)
  "Does alpha-beta search. 
is-maxs-turn-p takes one argument (a game) and returns true if it's max's turn in that game.  
expand takes one argument (a game) and gives all the immediate child games for this game.
terminal-p takes one argument (a game) and returns true if the game is over or not.
evaluate takes TWO arguements (a game and the is-maxs-turn-p function) and provides a quality
assessment of the game for 'max', from min-wins (-1000) to max-wins (1000)."

;;; IMPLEMENT ME
  (declare (type fixnum current-depth max-depth))
  ;; Base case:
  (if (or (funcall terminal-p game) (>= current-depth max-depth))
      ;; Evaluate(s)
      (funcall evaluate game is-maxs-turn-p)
      ;; Max's turn
      (if (funcall is-maxs-turn-p game)
          ;; sorting children before alpha-beta traversing
          (let ((sorted-children (sort-child-nodes (funcall expand game)
                                                   t ;; descending order
                                                   is-maxs-turn-p)))

            ;; iterate through immediate the children
            (dolist (c sorted-children)
              (setf alpha (max alpha (alpha-beta c
                                                 (1+ current-depth)
                                                 max-depth
                                                 is-maxs-turn-p
                                                 expand
                                                 terminal-p
                                                 evaluate
                                                 alpha
                                                 beta)))
              ;; check if alpha and beta have crossed
              (if (>= alpha beta)
                  (return-from alpha-beta beta)))
            alpha)
          ;; else, Min's turn
          ;; sorting children beofre alpha-beta traversing
          (let ((sorted-children (sort-child-nodes (funcall expand game)
                                                   nil ;; ascending order
                                                   is-maxs-turn-p)))
            ;; iterate through the immediate children
            (dolist (c sorted-children)
              (setf beta (min beta (alpha-beta c
                                         (1+ current-depth)
                                         max-depth
                                         is-maxs-turn-p
                                         expand
                                         terminal-p
                                         evaluate
                                         alpha
                                         beta)))
              ;; check if alpha and beta have crossed
              (if (>= alpha beta)
                  (return-from alpha-beta alpha)))
            beta))))




;; HELPER FUNCTION:
(defun sort-child-nodes (children max-turn? is-maxs-turn-p)
  "Takes in a list of children boards, t if it's max turn or nil if it's
min turn, and a function to determine turn from the board. SORT-CHILD-NODE
returns a sorted list of children boards in ascending order (min's turn)
or in descending order (max's turn)"
  (let (scored-children) ;; make a copy of children
    ;; iterate through the children boards
    (dolist (c children)
      ;; add children board and its evaluated scores to a new list
      (push (list c (evaluate c is-maxs-turn-p)) scored-children))
    ;; SORTING
    (if max-turn?
        ;; Max's turn: sort by scores in descending order
        (setf scored-children (stable-sort scored-children #'> :key #'second))
        ;; Min's turn: sort by scores in ascending order
        (setf scored-children (stable-sort scored-children #'< :key #'second)))
    ;;    filter out only the children board and return them
    (mapcar #'first scored-children)))






;; HELPER FUNCTION
(defun count-vertical (game color x y)
  "Take in the position of a blank square with x and y- coordination, a game state, and a color.
Then count the number of checkers (of color) vertically bellow the blank square"
  (if (= y 0)
      0
      (let ((count 0)
            (b (board game)))
        (loop for row downfrom (1- y)   ;;;; exit if out of the board dimension
                thereis (< row 0)
              do (if (= (aref b x row) color) ;; if same color, keep counting
                     (incf count)
                     (return-from count-vertical count))) ;; different color seen -> bail
        count)))

;; HELPER FUNCTION
(defun count-horizontal (game color x y)
  "Take in the position of a blank square with x and y- coordination, a game state, and a color.
Then count the number of checkers (of color) in rows on both side of  the blank square"
  (let ((count 0)
        (w (width game))
        (b (board game)))

    (block to-left
      (loop for column downfrom (1- x)
              thereis (< column 0)       ;; exit if out of the board dimension 
            do (if (= (aref b column y) color) ;; if same color, keep counting
                   (incf count)
                   (return-from to-left count)))) ;; different color seen -> bail
    (block to-right
      (loop for column from (1+ x)
              thereis (>= column w)       ;; exit if column is larger than the width
            do (if (= (aref b column y) color) ;; if same color, keep counting
                   (incf count)
                   (return-from to-right count)))) ;; different color seen -> bail
    count))



;; HELPER FUNCTION
(defun count-diagonal-1 (game color x y)
  "Take in the position of a blank square with x and y- coordination, a game state, and a color.
Then count the number of checkers (of color) in a row on the same diagonal line with the blank
Direction: top right to bottom left"
  (let ((count 0)
        (w (width game))
        (h (height game))
        (b (board game)))
    ;; start from the blank square of (x, y), count towards the top-right corner
    (block to-top-right
      (loop for i from 1
              thereis (or (>= (+ x i) w) (>= (+ y i) h))   ;; exit if out of the board dimension
            do (if (= (aref b (+ x i) (+ y i)) color) ;; if same color, keep counting
                   (incf count)
                   (return-from to-top-right count)))) ;; different color seen -> bail
    ;; start from the blank square of (x, y), count towards the bottom-left corner
    (block to-bottom-left
      (loop for i from 1
              thereis (or (< (- x i) 0) (< (- y i) 0))    ;; exit if out of the board dimension
            do (if (= (aref b (- x i) (- y i)) color)  ;; if same color, keep counting
                   (incf count)
                   (return-from to-bottom-left count)))) ;; different color seen -> bail
    count))

;; HELPER FUNCTION
(defun count-diagonal-2 (game color x y)
  "Take in the position of a blank square with x and y- coordination, a game state, and a color.
Then count the number of checkers (of color) in a row on the same diagonal line with the blank
Direction: top left to bottom right"
  (let ((count 0)
        (w (width game))
        (h (height game))
        (b (board game)))
    ;; start from the blank square of (x, y), count towards the top-left corner
    (block to-top-left
      (loop for i from 1
              thereis (or (< (- x i) 0) (>= (+ y i) h))   ;; exit if out of the board dimension
            do (if (= (aref b (- x i) (+ y i)) color) ;; if same color, keep counting
                   (incf count)
                   (return-from to-top-left count)))) ;; different color seen -> bail
    ;; start from the blank square of (x, y), count towards the bottom-right corner
    (block to-bottom-right
      (loop for i from 1
              thereis (or (>= (+ x i) w) (< (- y i) 0))   ;; exit if out of the board dimension
            do (if (= (aref b (+ x i) (- y i)) color) ;; if same color, keep counting
                   (incf count)
                   (return-from to-bottom-right count)))) ;; different color seen -> bail
    count))
                               
                               


;; HELPER FUNCTION
(defun valid-move-score (game color count-function)
  "For counting vertically: take a game state, color and the counting function.
This function will iterate through all the blank spaces generated by the function
row-for-move and calculate the number of checkers in row in the direction of
the counting function."
  (let ((overall 0)
        (w (width game)))
    ;; iterate through all blank squares generated by row-for-move only
    (dotimes (column w)
      (let ((row (row-for-move column game)))
        ;; if the row returned is valid (row != -1), then start counting
        (if (>= row 0)
            ;; collect the score into the overall sum
            ;; score is weighted by 10^n
            (incf overall (expt 10 (funcall count-function game color column row))))))
    ;; return the overall sum of the number of checkers in a row in a board
    overall))


  
;; HELPER FUNCTION
(defun new-max (game)
  "Calculate the new possible max-wins of a game state"
  (expt 10 (/ (* (width game) (height game)) 2)))

;; HELPER FUNCTION
(defun scale (score game)
  "Scale the score to a range that is within (-1000,1000)"
  ;; the chosen range is (-500, 500)
  (let* ((old-max (new-max game))
        (old-min (- old-max)))
    (+ -500 (/ (* (- score old-min) (- 500 -500)) (- old-max old-min)))))





;; HELPER FUNCTION
(defun score-color (game)
  "Score the game state"
  ;; Adding all scores from each direction for each color into a total sum
  ;; score is scaled to be within the range smaller than (-1000, 1000)
  (scale (+ (valid-move-score game black #'count-vertical)
            (valid-move-score game black #'count-horizontal)
            (valid-move-score game black #'count-diagonal-1)
            (valid-move-score game black #'count-diagonal-2)
            (* -1 (+ (valid-move-score game red #'count-vertical)
                     (valid-move-score game red #'count-horizontal)
                     (valid-move-score game red #'count-diagonal-1)
                     (valid-move-score game red #'count-diagonal-2))))
         game))




(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
is-maxs-turn-p is a function which, when called and passed the game, returns true
if it's max's turn to play."
  (let ((end (game-over game)))
    (if (null end) ;; game not over yet

	(progn

	  ;;; IMPLEMENT ME
	  ;; in this block, do your code which returns a heuristic
	  ;; evaluation of the system.  Feel free to create an outside function
	  ;; and call it if you don't want all the code here.

	  ;;0   ;; by default we're returning 0 (draw).  That's obviously wrong.





      ;; STUDENT's NOTE OF EVALUATION FUNCTION:
      ;; the evaluation here is rather simple due to all of the above helper functions
      ;; The overall score of a game state is calculated in the score-color, but basically
      ;; they are the sum of the weighted number of checkers in a row. Black's score is positive
      ;; and Red's score is negative before adding together.
      ;;
      ;; However, the perspective of max and min change as the turn changes, the following line
      ;; of code flip the score depending on the max-min perspective of the color
      (if (funcall is-maxs-turn-p game)
          ;; Max's turn
          (if (= (turn game) 1)
              (score-color game)
              (* -1 (score-color game)))
          ;; Min's turn
          (if (= (turn game) 1)
              (* -1 (score-color game))
              (score-color game))))


    
	  ;;; END IMPLEMENTATION
    

	


	(if (= 0 end)  ;; game is a draw
	    0

	    ;; else, the game is over but not a draw.  Return its value.
	    ;;
	    ;; this is a deep-math way of saying the following logic:
	    ;;
	    ;; if black, then if turn=black and ismaxsturn, then max-wins
	    ;; if black, then if turn=red and ismaxsturn, then min-wins
	    ;; if black, then if turn=black and !ismaxsturn, then min-wins
	    ;; if black, then if turn=red and !ismaxsturn, then max-wins
	    ;; if red, then if turn=black and ismaxsturn, then min-wins
	    ;; if red, then if turn=red and ismaxsturn, then max-wins
	    ;; if red, then if turn=black and !ismaxsturn, then max-wins
	    ;; if red, then if turn=red and !ismaxsturn, then min-wins
	    ;;
	    ;; keep in mind that black=1, red=-1, max-wins=1000, red-wins = -1000

	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))



;;I've decided to make this function available to you

(defun make-computer-move (game depth)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
the one which had the highest value for max., and making that move and returning the new game."

  (let* ((max (turn game)))
    (max-element (all-moves game)
		 (lambda (g)
		   (alpha-beta g 0 depth 
			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
			       (lambda (gm) (all-moves gm)) ;; expand
			       (lambda (gm) (game-over gm)) ;; terminal-p
			       #'evaluate ;; evaluate
			       min-wins
			       max-wins)))))





;; go back to cl-user
(in-package :cl-user)
