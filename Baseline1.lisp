; Title          : SARS.lisp
; Team           : Renat Khalikov
; Created on     : April 25, 2018
; Description    : Baseline Mastermind player that plays in tournaments.
;                  Based off of Baseline stragety #1.
;                  The system chooses a "secret code" and program tries to
;                  guess that code.
; Purpose        : baseline against which we can compare the performance of
;                  our more sophisticated strategy.
; Usage          : (play-tournament *Mastermind* 'SARS 'insert-colors 25)
; Build with     : (load (compile-file "/Users/renatkhalikov/Desktop/code/project/SARS.lisp"))

(defvar *PermutationList* NIL)
(defvar *Iterator* -1)


; Permutations( ) generates a list of all permutations of list colors in
; lexicographic order, based of stackoverflow question:
;https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp
;
; board: contains a number representing # of pegs in the game
; colors: a list containing colors in the game
(defun Permutations (colors board)
  (cond ((equal 0 board) (list nil))
        ((null colors) nil)
        ((null (cdr colors)) (list colors))
        (t (loop for element in colors
             append (mapcar (lambda (l) (cons element l))
                            (Permutations colors (- board 1)))))))

; AllPossibilities( ) set up function that calls Permutations( )
;
; board: a number representing # of pegs in the game
; colors: a list containing colors in the game
(defun AllPossibilities (board colors)
  (setf *PermutationList* (Permutations colors board)))

; SARS( ) general-purpose player, baseline strategy #1
; Exhaustively enumerate all possibilities.
; Guess each possibility in lexicographic order one at a time,
; paying no attention to the system’s responses (in this case,
; last-response is used to initialize Iterator if it is NIL).
;
; board: a number representing # of pegs in the game
; colors: a list containing colors in the game
; SCSA: a code that has been generated using some fixed number of
;       pegs and colors
; last-response: shows the previous guess
(defun SARS (board colors SCSA last-response)
  (declare (ignore SCSA))
  ; if a new tournament has started (last-response is nil),
  ; initialize iterator to the begining of PermutationList and
  ; initialize PermutationList by calling AllPossibilities function
  (if (not last-response)
    (progn
      (setf *Iterator* -1)
      (AllPossibilities board colors)))
  ; increment the Iterator by 1
  (setf *Iterator* (1+ *Iterator*))
  ;(print (nth *Iterator* *PermutationList*))
  ; nth function returns a permutation in PermutationList that
  ; is in position Iterator
  (nth *Iterator* *PermutationList*))
