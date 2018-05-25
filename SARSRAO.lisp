; Title          : SARS.lisp
; Author         : Renat Khalikov
; Created on     : May 7, 2018
; Description    : Mastermind player that plays in tournaments. Based off of
;                  Baseline stragety #3 + RAO algorigthm.
;                  The system chooses a "secret code" and program tries to
;                  guess that code.
; Purpose        : our more sophisticated strategy, correctly guesses code for
;                  10 10 insert-colors mastermind tournament.
; Usage          : (play-tournament *Mastermind* 'SARS 'insert-colors 25)
; Build with     : (load (compile-file "/Users/renatkhalikov/Desktop/code/project/SARS.lisp"))

(defvar *Iterator* -1)
(defvar *MonochromaticList* NIL)    ; Contains monochromatic list (A A A),
                                    ; (B B B), (C C C)...
(defvar *PossibleColors* NIL)       ; Contains correct colors (but wrong
                                    ; position after running baseline#3)

; RAO algorithm variables
(defvar *ColorCount* NIL)           ; Holds a list counting how many times a
                                    ; color in PossibleColors appears

(defvar *AvailableSpots* NIL)       ; Tells me index of available positions
                                    ; left in the final answer, positions
                                    ; that have not yet been checked
                                    ; by RAO's algorithm

(defvar *ColorPosition* NIL)        ; Keeps track of colors correct position
                                    ; once their correct spot has been found

(defvar *RaoColorBoard* NIL)        ; A list of colors that do not appear in
                                    ; the answer (gives a response of 0,
                                    ; used find colors correct postion
                                    ; by checking last-response

(defvar *RaoColor* NIL)             ; The color that does not appear in
                                    ; the final answer
(defvar *AllColors* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(defvar *IteratorAvailableSpot* 0)
(defvar *CorrectAnswer* NIL)        ; Holds the correct answer after
                                    ; running RAO's algorithm

; flatten( ) gets rid of nested parenthesis
;
; list: a list containing nested parenthesis
;https://stackoverflow.com/questions/47228365/get-the-elements-from-a-nested-list-in-lisp?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
(defun flatten (list)
  (cond
    ((null list)
     nil)
    ((consp (car list))
     (nconc (flatten (car list)) (flatten (cdr list))))
    ((null (cdr list))
     (cons (car list) nil))
    (t
     (cons (car list) (flatten (cdr list))))))

; BuildCorrectAnswer( ) reads ColorPosition and builds correct answer from it
; and stores it in CorrectAnswer
; ColorPosition must have correct colors in their correct positions
;
; board: a number representing # of pegs in the game
; ColorPosition: a list representing colors in their correct position
; CorrectAnswer: a list that contains the correct final guess
(defun BuildCorrectAnswer (board)
  (setf *CorrectAnswer* (make-list board))
  (loop for i in *ColorPosition*
    append (loop for j in (flatten (cdr i))
      do (setf (nth j *CorrectAnswer*) (car i))))
  *CorrectAnswer*)

; FindCorrectColorPosition( ) inserts correct color into RaoColorBoard
; if the correct color has more instances that appear in the answer
;
; colorandcount: contains the correct color to insert and the count of how
;                many instances of that color are left
; *IteratorAvailableSpot*: interator to tranverse AvailableSpots
; RaoColorBoard: list contains a color that is not in the final answer
(defun FindCorrectColorPosition (colorandcount)
  (if (flatten (cdr colorandcount))
    (setf (nth *IteratorAvailableSpot* *RaoColorBoard*) (car colorandcount))))

; MakeAGuess( ) calls FindCorrectColorPosition and passes next available color
; and the number of instances of that color
;
; ColorCount: contains a list of correct colors to insert and their instances
; RaoColorBoard: list contains a color that is not in the final answer
(defun MakeAGuess ()
  (FindCorrectColorPosition (car *ColorCount*))
  *RaoColorBoard*)

; GetNextColorInColorCount( ) resets IteratorAvailableSpot to 0 and deletes
; color that doesnt have any more instances in ColorCount
;
; ColorCount: contains a list of correct colors to insert and their instances
; *IteratorAvailableSpot*: interator to tranverse AvailableSpots
; RaoColorBoard: list contains a color that is not in the final answer
(defun GetNextColorInColorCount ()
  (setf *IteratorAvailableSpot* 0)
  (setf *ColorCount* (cdr *ColorCount*)))

; MoveColorToNextAvailableSpot( ) resets colors in RaoColorBoard and
; increments the interator
;
; *IteratorAvailableSpot*: interator to tranverse AvailableSpots
; RaoColorBoard: list contains a color that is not part of the final answer
; RaoColor: a color that is not part of the final answer
(defun MoveColorToNextAvailableSpot ()
  (setf (nth *IteratorAvailableSpot* *RaoColorBoard*) *RaoColor*)
  (setf *IteratorAvailableSpot* (1+ *IteratorAvailableSpot*)))

; SubtractCountFromColorCount( ) once colors position has been found, decrement
; the colors instance in ColorCount
;
; color: color that needs its instance decremented
; ColorCount: contains a list of correct colors to insert and their instances
(defun SubtractCountFromColorCount (color)
  (loop for i in *ColorCount*
    do (if (equal (car i) color)
      (progn
        (if (= (car (flatten (cdr i))) 1)
          (setf (nth (position i *ColorCount* :test #'equal) *ColorCount*) (list color (list 0))))
        (if (> (car (flatten (cdr i))) 1)
          (setf (nth (position i *ColorCount* :test #'equal) *ColorCount*) (list color (list (1- (car (flatten (cdr i))))))))))))

; SubtractCountFromColorCount( ) once colors position has been found, add its
; correct position to ColorPosition
;
; color: color that needs its position recorded
; ColorPosition: a list representing colors in their correct position
; *IteratorAvailableSpot*: interator to tranverse AvailableSpots
(defun AddCorrectPositionOfColorToColorPosition (color)
  (loop for i in *ColorPosition*
    do (if (equal (car i) color)
      (progn
        (if (= (car (flatten (cdr i))) -1)
          (setf (nth (position i *ColorPosition* :test #'equal) *ColorPosition*) (list color (list *IteratorAvailableSpot*)))
          (push *IteratorAvailableSpot* (cdr (last (car (cdr i))))))))))

; CountElementsInList( ) counts how many times an element appears in the list
;
; element: color that needs its count recorded
; a_list: a list containg PossibleColors
; https://stackoverflow.com/questions/18937885/counting-all-occurences-of-an-atom-inside-a-list-lisp?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
(defun CountElementsInList (element a_list)
  (cond
   ((null a_list) 0)
   ((equal element (car a_list)) (+ 1 (CountElementsInList element (cdr a_list))))
   (t (CountElementsInList element (cdr a_list)))))

; BuildList( ) builds a monochromatic list of all one color
;
; board: a number representing # of pegs in the game
; color: color that the list will be build from
(defun BuildList (board color)
  (loop for i from 1 to board
    collect color))

; InitializeRaoVariables( ) initializes Rao variables at the start of
; the algorithm
;
; a_list: a list containg PossibleColors
(defun InitializeRaoVariables (a_list)
  ; Initialize *ColorCount*
  (loop for color in (remove-duplicates a_list :test #'equal)
    do (if *ColorCount*
      (push (list color (list (CountElementsInList color *PossibleColors*))) (cdr (last *ColorCount*)))
      (push (list color (list (CountElementsInList color *PossibleColors*))) *ColorCount*)))

  ; Initialize *AvailableSpots*
  (loop for i from 0 to (1- (length a_list))
    do (if *AvailableSpots*
      (push i (cdr (last *AvailableSpots*)))
      (push i *AvailableSpots*)))

  ; Initialize *ColorPosition*
  (loop for color in (remove-duplicates a_list :test #'equal)
    do (if *ColorPosition*
      (push (list color (list -1)) (cdr (last *ColorPosition*)))
      (push (list color (list -1)) *ColorPosition*)))

  ; Initialize *RaoColorBoard*
  (loop for color in *AllColors*
    do (if (and (not (member color *PossibleColors*)) (not *RaoColorBoard*))
      (progn
        (setf *RaoColorBoard* (BuildList (length a_list) color))
        (setf *RaoColor* color)))))

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

; SARS( ) tournament player, baseline strategy #3
; make monochromatic guesses, this will tell you how many pegs of each color
; are in the answer (gives us the correct colors in the answer).
; Then perform Rao algorithm to find correct postion of correct colors in
; the answer.
;
; board: a number representing # of pegs in the game
; colors: a list containing colors in the game
; SCSA: a code that has been generated using some fixed number of
;       pegs and colors
; last-response: shows the previous guess
(defun SARS (board colors SCSA last-response)
  (declare (ignore SCSA))
  ; if a new tournament has started (last-response is nil),
  ; initialize all variables.
  (cond
    ((not last-response)
      (progn
        (setf *Iterator* -1)
        (setf *MonochromaticList* NIL)
        (setf *PossibleColors* NIL)
        (setf *ColorCount* NIL)
        (setf *AvailableSpots* NIL)
        (setf *ColorPosition* NIL)
        (setf *RaoColorBoard* NIL)
        (setf *IteratorAvailableSpot* 0)
        (setf *RaoColor* 0)
        (setf *CorrectAnswer* NIL)
        (Monochromatic board colors)
        (setf *Iterator* (1+ *Iterator*))
        (nth *Iterator* *MonochromaticList*)))

    ; flag to see if we need to build monochromatic list of colors
    ; if we do, this will build monochromatic list of colors
    ((/= *Iterator* 100)
      (if (> (car last-response) 0)
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
                  *PossibleColors*)))))
        (progn
          (if (< (1+ *Iterator*) (length colors))
            (progn
              (setf *Iterator* (1+ *Iterator*))
              (nth *Iterator* *MonochromaticList*))
            (progn
              (setf *Iterator* 100)
              *PossibleColors*)))))

    ; This will perform Rao's algorithm once monochromatic list has been built
    ; and PossibleColors have been found
    (t
      (if (not *AvailableSpots*)
        (progn
          (InitializeRaoVariables *PossibleColors*)
          (MakeAGuess))
        (if (NOT (equal *ColorCount* NIL))
          (progn
            (cond
              ((if (= *IteratorAvailableSpot* (1- board))
                (progn
                  (if (= (car last-response) 1)
                    (progn
                      (AddCorrectPositionOfColorToColorPosition (nth *IteratorAvailableSpot* *RaoColorBoard*))
                      (SubtractCountFromColorCount (nth *IteratorAvailableSpot* *RaoColorBoard*))
                      (setf (nth *IteratorAvailableSpot* *RaoColorBoard*) *RaoColor*)))
                  (GetNextColorInColorCount)
                  (MakeAGuess))))
              ((if (= (car last-response) 0)
                (progn
                  (MoveColorToNextAvailableSpot)
                  (MakeAGuess))))
              ((if (= (car last-response) 1)
                (progn
                  (AddCorrectPositionOfColorToColorPosition (nth *IteratorAvailableSpot* *RaoColorBoard*))
                  (SubtractCountFromColorCount (nth *IteratorAvailableSpot* *RaoColorBoard*))
                  (MoveColorToNextAvailableSpot)
                  (MakeAGuess))))))
          (BuildCorrectAnswer board))))))
