; Title          : SARS.lisp
; Team           : Renat Khalikov
; Created on     : May 7, 2018
; Description    : Baseline mastermind player that plays in tournaments.
;                  Based off of Baseline stragety #2.
;                  The system chooses a "secret code" and program tries to
;                  guess that code.
; Purpose        : baseline against which we can compare the performance of
;                  our more sophisticated strategy.
; Usage          : (play-tournament *Mastermind* 'SARS 'insert-colors 25)
; Build with     : (load (compile-file "/Users/renatkhalikov/Desktop/code/project/SARS.lisp"))

(defvar *PermutationList* NIL)
(defvar *LastPermutationList* NIL)
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

(defun SkipPermutations (LastResponseLetter CurrentGuess)
  (let ((flag 0))
  (loop for i in CurrentGuess
    if (and (eql LastResponseLetter i) (= flag 0))
    do
      (progn
        (setf *Iterator* (1+ *Iterator*))
        (setf flag 1)
        (SkipPermutations LastResponseLetter (nth *Iterator* *PermutationList*))))))

; SARS( ) general-purpose player, baseline strategy #2
; Exhaustively enumerate all possibilities.
; Guess each possibility in lexicographic order unless
; its last-response is 0.
; In that case, never again on that round make any guess with a
; list of colors that returned last-response of 0.
;
; board: a number representing # of pegs in the game
; colors: a list containing colors in the game
; SCSA: a code that has been generated using some fixed number of
;       pegs and colors
; last-response: shows the previous guess
(defun SARS (board colors SCSA last-response)
  (declare (ignore SCSA))
  ; if a new tournament has started (last-response is nil),
  ; initialize all global variables.
  (cond ((not last-response)
      (progn
        (setf *Iterator* -1)
        (setf *LastPermutationList* NIL)
        (AllPossibilities board colors)
        (setf *Iterator* (1+ *Iterator*))))

  ((and (= (car last-response) 0) (= (car (rest last-response)) 0))
      (progn
        (if *LastPermutationList*
          (let ((LastGuess (nth *Iterator* *PermutationList*)))
          (loop for i in LastGuess
            for y in *LastPermutationList*
            do (push i (cdr (last y)))))
          (let ((LastGuess (nth *Iterator* *PermutationList*)))
          (loop for i in LastGuess
            do (push (list i) *LastPermutationList*))))
        (setf *Iterator* (1+ *Iterator*))
        (loop for LastPermutation in *LastPermutationList*
          append (loop for i in LastPermutation
            do (SkipPermutations i (nth *Iterator* *PermutationList*))))))
  (t
    (progn
      (setf *Iterator* (1+ *Iterator*))
        (loop for LastPermutation in *LastPermutationList*
          append (loop for i in LastPermutation
            do (SkipPermutations i (nth *Iterator* *PermutationList*)))))))
  (nth *Iterator* *PermutationList*))
