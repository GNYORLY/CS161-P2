;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;if the current element is a list, append element to the end of FRINGE and call BFS on the tail
;else if the current element is the last element in the list, insert element into the empty tail
;else insert the current element into the tail and call BFS on the tail
(defun BFS (FRINGE)
    (cond ((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE))))
          ((null (cdr FRINGE)) (cons (car FRINGE) (cdr FRINGE)))
          (t (cons (car FRINGE) (BFS (cdr FRINGE))))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

(defun FINAL-STATE (S)
(cond ((equal S '(T T T T)) T)
(t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

;the function follows the rules below for each of the possible inputs of A, (h b d p)
;if (baby and dog) or (baby and poison) are alone on one side, return NIL
;(this will always return NIL if the current state S is invalid)
;    -> if the (second and third) or (second and fourth) elements in S have same sign, return NIL
;if homer not on the same side as the chosen entity, return NIL
;    -> if A and the first element of S do not have same sign, return NIL
;otherwise, move homer and the chosen entity to the other side
;    -> else switch the signs of the chosen entities

(defun NEXT-STATE (S A)
(cond ((equal A 'h) (cond ((equal (second S) (third S)) NIL)                  ;if h is chosen, Homer moves alone
                              ((equal (second S) (fourth S)) NIL)                 
                              ((equal (first S) T) (list (cons NIL (cdr S))))
                              (t (list (cons T (cdr S))))))
      ((equal A 'b) (cond ((and (equal (first S) T) (equal (second S) T))     ;if b is chosen and h is on the same side as b, the move is always valid
                                    (list (append '(NIL NIL) (cddr S))))
                              ((and (equal (first S) NIL) (equal (second S) NIL)) ;switch the first and second values of S to their opposite counterpart
                                    (list (append '(T T) (cddr S))))
                              (t NIL)))
      ((equal A 'd) (cond ((equal (second S) (fourth S)) NIL)                 ;for d, only invalid if b and p are on the same side or if h is not on same
                              ((and (equal (first S) T) (equal (third S) T))  ;side as d
                                    (list (cons NIL (cons (second S) (cons NIL (cdddr S))))))
                              ((and (equal (first S) NIL) (equal (third S) NIL))
                                    (list (cons T (cons (second S) (cons T (cdddr S))))))
                              (t NIL)))
      ((equal A 'p) (cond ((equal (second S) (third S)) NIL)                  ;for p, only invalid if b and d are on the same side or if h is not on same
                              ((and (equal (first S) T) (equal (fourth S) T)) ;side as p
                                    (list (cons NIL (cons (second S) (cons (third S) '(NIL))))))
                              ((and (equal (first S) NIL) (equal (fourth S) NIL))
                                    (list (cons T (cons (second S) (cons (third S) '(T))))))
                              (t NIL)))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

;appends next states from each possible operator, (h b d p), on state S
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

(defun ON-PATH (S STATES)
    (cond ((equal S (car STATES)) T)      ;if current element of STATES is equal to S, return T
    ((null (cdr STATES)) NIL)             ;else if at the last element of STATES, return NIL
    (t (ON-PATH S (cdr STATES)))))        ;else call ON-PATH on the tail of STATES


; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.

(defun MULT-DFS (STATES PATH)
    (cond ((equal (FINAL-STATE (car STATES)) T) (append PATH (list (car STATES)))) ;if element is goal state, return complete path
          ((null STATES) NIL)                                                      ;if no more elements in STATES, return NIL
          ((equal (DFS (car STATES) PATH) NIL) (MULT-DFS (cdr STATES) PATH))       ;call DFS on current element, if result is NIL go to the next state
          (t (DFS (car STATES) PATH))))                                            ;else return the result of the DFS call


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun DFS (S PATH)
    (cond ((equal (FINAL-STATE S) T) (append PATH (list S)))  ;if S is the goal state, return full PATH
          ((equal (ON-PATH S PATH) T) NIL)                    ;if S is already in PATH, return NIL
          ((null (SUCC-FN S)) NIL)                            ;if S has no successors, return NIL
          (t (MULT-DFS (SUCC-FN S) (append PATH (list S)))))) ;else call MULT-DFS on successors and add state to PATH

    
