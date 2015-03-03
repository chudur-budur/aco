;(defpackage :new (:use :common-lisp))

;(in-package :new)

 (defun random-elt (seq)
  "Returns a random element in a sequence"
  (elt seq (random (length seq))))

(defun fill-seq (seq func &key (pass-index nil))
  "Fill a sequence with a function.  If pass-index is non-NULL, then
the function is called as (func x) where x is the index in the sequence of
the element in question.  Otherwise (by default) the function is just called
as (func)."
  (dotimes (x (length seq) seq)
    (setf (elt seq x) (if pass-index (funcall func x) (funcall func)))))

(defun tournament-select (elts &key (fitness #'identity) (better #'<) (size 2))
  "Performs a tournament selection of the given size on the provided sequence,
with the provided fitness function provided.  Higher fitness is preferred.  The default
fitness is #'identity"
  (let ((fsize (floor size)))
    (if (= size fsize)  ;; it's an integer
	(let* ((len (length elts)) (e (elt elts (random len))))
	  (dotimes (i (- fsize 1) e)  ;; in case size isn't an integer but is a real like 2.0
	    (let ((new (elt elts (random len))))
	      (if (funcall better (funcall fitness new) (funcall fitness e))
		  (setf e new)))))
	(if (< (random 1.0) (- size fsize))  ;; non-intenger -- convert to integer
	    (tournament-select elts :fitness fitness :size (ceiling size))
	    (tournament-select elts :fitness fitness :size (floor size))))))


(defstruct (problem 
	     (:print-function (lambda (struct stream depth)
				(declare (ignore depth))
				(format stream "[~a:" (problem-max struct))
				(map nil (lambda (i w v) (format stream " <~a: ~a ~a>" i w v))
				     (problem-indexes struct)
				     (problem-weights struct) 
				     (problem-values struct))
				(format stream "]"))))
  (weights nil)  ;; list of weights, one per element
  (values nil)   ;; list of values, one per element
  (indexes nil)  ;; list of index numbers, increasing, one per element
  (size 0)       ;; total number of elements
  (max 0)        ;; maximum allowed weight
  (pheromones nil)  ;; pheromone table
  (evals nil)    ;; total number of evaluations
  (counts nil))  ;; pheromone update count table

(defun print-pheromones (problem)
  "Prints the single- and paired- pheromone tables."
  (print 'PHEROMONES)
  (print (problem-pheromones problem))
  (print 'COUNTS)
  (print (problem-counts problem))
  (print 'EVALS)
  (print (problem-evals problem)))


;; (defun reset-problem (struct)
;;   "Build the single and paired pheromone tables, intially zero on all slots"
;;   (let ((size (problem-size struct)))
;;     (setf (problem-pheromones struct) (make-array size :initial-element 0))
;;     (setf (problem-evals struct) (make-array size :initial-element 0))
;;     (setf (problem-counts struct) (make-array size :initial-element 0))))


(defun reset-problem (struct initial-pheromone)
  "Build the single and paired pheromone tables, intially zero on all slots"
  (let ((size (problem-size struct)))
    (setf (problem-pheromones struct) 
	  (fill-seq (make-array size :initial-element 0)
		    (lambda () initial-pheromone)))
    (setf (problem-evals struct) (make-array size :initial-element 0))
    (setf (problem-counts struct) (make-array size :initial-element 0))))


(defun load-problem (stream)
  "The problem should start with the number of elements,
followed by each element consisting of three numbers,
finally followed by the maximum weight.  The thre numbers
are the element index, the value of the element, and the weight
of the element.  Builds a PROBLEM structure."
  (let ((p (make-problem)) weights values indexes (size (read stream)))
    (setf (problem-size p) size)
    (dotimes (x size)
      (read stream)  ;; throw away
      (push (read stream) values)
      (push (read stream) weights)
      (push x indexes))
    (setf (problem-weights p) (make-array size :initial-contents (reverse weights)))
    (setf (problem-values p)  (make-array size :initial-contents (reverse values)))
    (setf (problem-indexes p) (reverse indexes))
    (setf (problem-max p) (read stream))    
    p))


(defun valid-component-p (problem solution solution-weight component)
  "Returns TRUE if the component is valid to add to a solution at present, given the problem, 
current (possibly incomplete) solution, and total weight of that solution."
  ;; By default: a component is valid if it's not a member of the solution already
  ;; and adding it would not increase the weight beyond the problem maximum weight
  (and (not (member component solution))
       (<= (+ (aref (problem-weights problem) component)
	      solution-weight)
	   (problem-max problem))))

(defun valid-components (problem solution)
  "Returns a list of all components valid to add to a solution at present, given the problem.
Components are index numbers."
  (let ((solution-weight 
	 (apply #'+ (mapcar (lambda (c) (aref (problem-weights problem) c)) solution))))
    (remove-if-not (lambda (c) (valid-component-p problem solution solution-weight c))
		   (problem-indexes problem))))
	      
(defun desirability (problem component alpha beta)
  "Returns the 'desirability' -- my term for the value of the component that should be used for doing tournament selection.  By default this is simply the pheromone value."
  (* 
   (expt (aref (problem-pheromones problem) component) alpha)
   (expt (aref (problem-values problem) component) beta)))


(defun select-component (problem solution tournament-size minimum-counts alpha beta)
  "Selects a component for inclusion in a solution given a problem.  Returns NIL
if there is no valid component to add."
  ;; pick randomly among the valid components for the moment  -- FIXME
  (let ((v (valid-components problem solution)))
    (when v 
      (tournament-select v :size tournament-size
			 :better (lambda (c1 c2)
				   (cond 
				     ;; if c1 is below the minimum but not c2
				     ((and (< (aref (problem-counts problem) c1) minimum-counts)
					   (>= (aref (problem-counts problem) c2) minimum-counts))
				      c1)
				     ;; if c2 is below the minimum but not c1
				     ((and (>= (aref (problem-counts problem) c1) minimum-counts)
					   (< (aref (problem-counts problem) c2) minimum-counts))
				      c2)
				     ;; if c1 is worse than c2
				     ((< (desirability problem c1 alpha beta) (desirability problem c2 alpha beta))
				      c2)
				     ;; if c2 is worse than c1
				     ((> (desirability problem c1 alpha beta) (desirability problem c2 alpha beta))
				      c1)
				     ;; if c1 has been tested less than c2
				     ((< (aref (problem-counts problem) c1) (aref (problem-counts problem) c2))
				      c1)
				     ;; if c2 has been tested less than c1
				     ((> (aref (problem-counts problem) c1) (aref (problem-counts problem) c2))
				      c2)
				     ;; flip a coin
				     ((= 0 (random 2)) 
				      c1)
				     (t c2)))))))
       

(defun build-solution (problem tournament-size minimum-counts alpha beta)
  "Iteratively builds a solution to a problem and returns it.  A solution
is a list of component indexes -- the earlier indexes were added later."
  (let (solution)
    (loop
       (let ((component (select-component problem solution tournament-size minimum-counts alpha beta)))
	 (if component 
	     (push component solution) 
	    ;; (setf solution nil))))
	     (return))))
    solution))

(defparameter neg-inf most-negative-double-float)

(defun solution-fitness (problem solution)
  "Returns the fitness of a solution for a given problem."
  ;; We're using the sum total values for each component in the solution.
  (let ((sum 0))
    (dolist (c solution sum)
      (incf sum (aref (problem-values problem) c)))))


(defun update-pheromones (problem solutions fitnesses learning-rate)
  "Updates the pheromone values to reflect the provided solutions."
  (let ((delta (make-array (problem-size problem) :initial-element neg-inf)))
    (dotimes (c (problem-size problem))  ;; for each component
      (mapc (lambda (s f)  ;; for each solution and its component
	      (when (member c s)
		(incf (aref (problem-evals problem) c))
		(setf (aref delta c) (max (aref delta c) f))))
	    solutions fitnesses)
      (when (> (aref delta c) neg-inf)  ;; used at least once
	(incf (aref (problem-counts problem) c))
	(let ((alpha (max (/ 1 (aref (problem-counts problem) c)) learning-rate)))
	  (setf (aref (problem-pheromones problem) c)
		(+ (* (- 1 alpha) (aref (problem-pheromones problem) c))
		   (* alpha (aref delta c)))))))))

(defun new (problem &key (iterations 100) (popsize 500) (tournament-size 2) (tournament-increment 0) (learning-rate 0.1) (minimum-counts 1) (initial-pheromone 5000) (alpha 1) (beta 2))
  "Each iteration, builds POPSIZE solutions, then updates the pheromones on those solutions.
Returns the best discovered solution."
  (let (best)
    (reset-problem problem initial-pheromone)
    (dotimes (i iterations (sort (copy-list best) #'<))
      (let ((solutions (fill-seq (make-list popsize) (lambda () (build-solution problem tournament-size minimum-counts alpha beta)))))
	(let ((fitnesses (mapcar (lambda (s) (solution-fitness problem s)) solutions)))
	  (dolist (s solutions)
	    (if (or (null best) (< (solution-fitness problem best) (solution-fitness problem s)))
		(setf best s)))
	  (update-pheromones problem solutions fitnesses learning-rate)
	  ;(print-pheromones problem)
	  (incf tournament-size tournament-increment)
	  (format t "~%~a -> ~a" (solution-fitness problem best) (sort (copy-list best) #'<)))))))

(defvar *p* nil)
(with-open-file (s "knapsacks/100.in" :direction :input)
  (setf *p* (load-problem s)))
   
;(in-package :common-lisp)
