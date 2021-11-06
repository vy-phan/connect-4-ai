;;;; Connect-Four Player
;;;; Due Midnight, the evening of WEDNESDAY, APRIL 28.
;;;;
;;;;
;;;; You will provide the TA a single file (a revised version of this one).
;;;; You'll write the heuristic board evaluation function and the alpha-beta searcher
;;;; for a CONNECT FOUR game.  If you don't know how to play connect four, and can't
;;;; find stuff online, get ahold of me.  The version of connect four we'll be playing
;;;; is NON-TOROIDAL (non-"wraparound").
;;;;
;;;;
;;;;
;;;; HOW TO USE THIS TEMPLATE FILE
;;;;
;;;; This is the file you will fill out and provide to the TA.  It contains your project-specific
;;;; code.  Don't bother providing the connectfour.lisp file -- we have it, thanks.
;;;;
;;;; I'm providing you with one function the MAKE-COMPUTER-MOVE function.  This function 
;;;; takes a game state and generates all one-move-away states from there.  It then calls 
;;;; ALPHA-BETA on those states returns the new game state which got the highest alpha-beta 
;;;; score.  Piece of cake.
;;;;
;;;; You will write:
;;;;
;;;; 1. The ALPHA-BETA function.  This is a straightforward implementation of the
;;;;    function described in the lecture notes.  However to make your player better
;;;;    you MIGHT (or might not! -- it could make things worse) sort the expanded children
;;;;    based on who's best, so as to increase the chance that alpha-beta will cut off
;;;;    children early and improve your search time.  Look into the SORT function.
;;;;
;;;; 2. The EVALUATE function.  Or more specifically, you'll fill out one part of
;;;;    it (the progn block shown).  Here you'll provide an intelligent heuristic
;;;;    evaluator.  Right now the evaluator provided ("0") literally says that
;;;;    all unfinished games are draws.  That's pretty stupid -- surely you can
;;;;    do better than that!  You're welcome, and perhaps encouraged, to write your
;;;;    own separate function(s) and call it from within the EVALUATE function so as
;;;;    to keep the EVALUATE function code clean.  Keep whatever functions and
;;;;    auxillary code to within this file please.
;;;;
;;;; Your code should work for any width or height board, and for any number of
;;;; checkers in a row to win (at least 2).  These values can be extracted from
;;;; game board functions in the connectfour.lisp file.  Indeed, I'd strongly suggest
;;;; examining all of that file as it may contain some valuable functions and constants
;;;; that you might want to know about.
;;;;
;;;; Your code will not only be in this file but will be in its own package as well.
;;;; Your package name will be :firstname-lastname
;;;; For example, the package name for MY package is called :sean-luke
;;;; will change the text :TEMPLATE with your :firstname-lastname package name
;;;; in the TWO places it appears in this file.
;;;;
;;;; You will then name your file "firstname-lastname.lisp".  For example, I would
;;;; name my own file "sean-luke.lisp"
;;;;
;;;;
;;;; RUNNING THE CODE
;;;;
;;;; You'll want to compile your code first, like I do to my code in this example:
;;;;
;;;; (load "connectfour")
;;;; (compile-file "connectfour.lisp")
;;;; (load "connectfour")
;;;; (load "sean-luke")
;;;; (compile-file "sean-luke.lisp")
;;;; (load "sean-luke")
;;;;
;;;; Now we're ready to go.  If you want to just do a quick human-human game, you
;;;; don't need to compile and load your own file, just the connectfour file.
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
;;;;
;;;;
;;;; TESTING POLICY
;;;;
;;;; Plagiarism rules still apply (no code given, no code borrowed from anyone or any source
;;;; except the TA and the Professor, do not lay eyes on other people's code anywhere, including
;;;; online).  However they are softened in the following fashion:
;;;;
;;;;    - You are strongly encouraged to play against each other as long as neither of you
;;;;      looks at each others' code nor discusses your alpha-beta code.  Go on the forum and
;;;;      ask for partners.
;;;;
;;;;    - You are NOT permitted to discuss, in any way, the ALPHA-BETA function with anyone
;;;;      except ME and the TA.  That has to be done entirely on your own. 
;;;;
;;;;    - You ARE allowed to discuss, in general and HIGHLY ABSTRACT terms, your own approach
;;;;      to the EVALUATE function.  You are not allowed to show code or read another student's
;;;;      code.  But you can talk about your theory and general approach to doing board evaluation
;;;;      for connect four.  Let a thousand heuristics bloom!  You are also welcome to
;;;;      go online and get ideas about how to do the EVALUATE function as long as you do NOT
;;;;      read code.  I'm just as good as the next person -- perhaps even better -- at gathering
;;;;      every scrap of connect four code on the web and comparing your code against it.
;;;;
;;;; OTHER POLICIES
;;;;
;;;; - This code may have, and almost certainly has, bugs.  I'll post revisions as needed.
;;;;
;;;; - Near the end of the exam we will have a single-elimination tournament competition.
;;;;      The winner of the competition gets to play against the professor's "better" evaluator
;;;;      (assuming I've written one by then) and ALSO receives an improvement in his overall
;;;;      course grade.  Runners up may receive something smaller: at least a candy bar, perhaps
;;;;      a better incentive.
;;;;
;;;; - If your evaluator is too slow, you will be penalized with a cut in your search depth.
;;;;   Thus it's in your best interest to have as good an evaluator/depth combination as possible.
;;;;   Note that it's often the case that evaluators that are good on ODD depths are bad on EVEN
;;;;   depths (or vice versa!).  I won't say what depth will be used in the tournament.
;;;;
;;;; - To make your code run as fast as possible, I strongly suggest you look into Lisp compiler optimization.  SBCL's profiler might prove helpful too.
;;;;
;;;; Good luck!
;;;;
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

;; (defun make-computer-move (game depth)
;;   "Makes a move automatically by trying alpha-beta on all possible moves and then picking
;; the one which had the highest value for max., and making that move and returning the new game."

;;   (let* ((max (turn game)))
;;     (max-element (all-moves game)
;; 		 (lambda (g)
;; 		   (alpha-beta g 0 depth 
;; 			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
;; 			       (lambda (gm) (all-moves gm)) ;; expand
;; 			       (lambda (gm) (game-over gm)) ;; terminal-p
;; 			       #'evaluate ;; evaluate
;; 			       min-wins
;; 			       max-wins)))))



(defun make-computer-move (game depth)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
the one which had the highest value for max., and making that move and returning the new game."

  (let* ((max (turn game)))
    (print "Picked child's score")
    (print (max-element (all-moves game)
		                (lambda (g)
                          (print "-------------------------------------------")
                   (print-game g)
		   (print (alpha-beta g 0 depth 
			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
			       (lambda (gm) (all-moves gm)) ;; expand
			       (lambda (gm) (game-over gm)) ;; terminal-p
			       #'evaluate ;; evaluate
			       min-wins
			       max-wins)))))))



;; go back to cl-user
(in-package :cl-user)
