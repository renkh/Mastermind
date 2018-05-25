; Title          : SARS.lisp
; Author         : Renat Khalikov
; Created on     : May 7, 2018
; Description    : Baseline mastermind player that plays in tournaments.
;                  Based off of Baseline stragety #3.
;                  The system chooses a "secret code" and program tries to
;                  guess that code.
; Purpose        : baseline against which we can compare the performance of
;                  our more sophisticated strategy.
; Usage          : (play-tournament *Mastermind* 'SARS 'insert-colors 25)
; Build with     : (load (compile-file "/Users/renatkhalikov/Desktop/code/project/SARS.lisp"))

(defvar *PermutationList* NIL)
(defvar *Iterator* -1)
(defvar *MonochromaticList* NIL)
(defvar *PossibleColors* NIL)
(defvar *PermListItr* -1)

; allColorpermutations( ) generates a list of all permutations of list colors in
; lexicographic order, based of stackoverflow question:
;https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp
;
; board: contains a number representing # of pegs in the game
; colors: a list containing colors in the game
(defun allColorpermutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (allColorpermutations (rest lst)))
            (allColorpermutations (append (rest lst) (list (first lst))) (rest remain))))))

; BuildList( ) builds a monochromatic list of all one color
;
; board: a number representing # of pegs in the game
; color: color that the list will be build from
(defun BuildList (board color)
  (loop for i from 1 to board
    collect color))

; BuildPossibleColorsList( ) adds colors to PossibleColors that have
; last responce of >0, last-responce will dictate how many colors to add to
; PossibleColors list
;
; board: a number representing how many occurances of color to add
; color: color that needs to be added to PossibleColors list
(defun BuildPossibleColorsList (board color)
  (if *PossibleColors*
    (setf *PossibleColors* (append *PossibleColors* (BuildList board color)))
    (progn
        (let ((a_list (BuildList board color)))
          (loop for x in a_list
            do (push x *PossibleColors*))))))

; BuildMonochromaticList( ) builds a monochromatic list of colors
;
; board: a number representing # of pegs in the game
; color: color that monochromatic list will be build with
(defun BuildMonochromaticList (board colors)
  (loop for i in colors
    do (if *MonochromaticList*
      (push (BuildList board i) (cdr (last *MonochromaticList*)))
      (push (BuildList board i) *MonochromaticList*))))

; BuildMonochromaticList( ) adds colors to PossibleColors that have
; last responce of >0, last-responce will dictate how many colors to add to
; PossibleColors list
;
; appearances: a number representing how many occurances of color to add
; color: color that needs to be added to PossibleColors list
(defun AddColorToPossibleColors (appearances colors)
  (BuildPossibleColorsList appearances (car colors)))

; Monochromatic( ) builds a monochromatic list of colors
;
; board: a number representing # of pegs in the game
; color: color that monochromatic list will be build with
(defun Monochromatic (board colors)
  (BuildMonochromaticList board colors))

; SARS( ) general-purpose player, baseline strategy #3
; Create a monochromatic list and test each monochromatic color. For colors
; of last-response greater than 0, add that color to a list of
; PossibleColors. PossibleColors will contain correct colors. Generate and
; test only answers consistent with PossibleColors color distribution.
;
; board: a number representing # of pegs in the game
; colors: a list containing colors in the game
; SCSA: a code that has been generated using some fixed number of
;       pegs and colors
; last-response: shows the previous guess
(defun SARS (board colors SCSA last-response)
  (declare (ignore SCSA))
  ; if a new tournament has started (last-response is nil),
  ; initialize all global variables
  (cond
    ((not last-response)
      (progn
        (setf *Iterator* -1)
        (setf *PermutationList* NIL)
        (setf *MonochromaticList* NIL)
        (setf *PossibleColors* NIL)
        (setf *PermListItr* -1)
        (Monochromatic board colors)
        (setf *Iterator* (1+ *Iterator*))
        (nth *Iterator* *MonochromaticList*)))

    ((> (car last-response) 0)
      (progn
        (if (< *Iterator* (length colors))
          (progn
            (AddColorToPossibleColors (car last-response) (nth *Iterator* *MonochromaticList*))
            (if (< (1+ *Iterator*) (length colors))
              (progn
                (setf *Iterator* (1+ *Iterator*))
                (nth *Iterator* *MonochromaticList*))
              (progn
                (setf *Iterator* 100)
                *PossibleColors*)))
          (progn
            (if (not *PermutationList*)
              (progn
                (setf *PermutationList* (allColorpermutations *PossibleColors*))
                (setf *PermutationList* (remove-duplicates *PermutationList* :test #'equal))))
            (setf *PermListItr* (1+ *PermListItr*))
            (nth *PermListItr* *PermutationList*)))))
    (t
      (progn
        (if (< (1+ *Iterator*) (length colors))
          (progn
            (setf *Iterator* (1+ *Iterator*))
            (nth *Iterator* *MonochromaticList*))
          (progn
            (setf *Iterator* 100)
            *PossibleColors*))))))

