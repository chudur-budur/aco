;(defpackage :as (:use :common-lisp))

;(in-package :as)


(defun fill-seq (seq func &key (pass-index nil))
  "Fill a sequence with a function.  If pass-index is non-NULL, then
the function is called as (func x) where x is the index in the sequence of
the element in question.  Otherwise (by default) the function is just called
as (func)."
  (dotimes (x (length seq) seq)
    (setf (elt seq x) (if pass-index (funcall func x) (funcall func)))))

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

(defun normalize (lis)
  "Normalizes a list.  An all-zero lis is replaced with 1/len for each element."
  (let ((sum (apply #'+ lis)) (len (length lis)))
    (if (> sum 0)
      (mapcar (lambda (e) (/ e sum)) lis)
      (mapcar (lambda (e) (declare (ignore e)) (/ 1 len)) lis))))

(defun roulette-selection (problem components alpha beta)
  ;; a linear search, not particularly fast
  (let ((probabilities (normalize (mapcar (lambda (c) (desirability problem c alpha beta)) components)))
	(i 0.0) 
	(target (random 1.0)))
    (map nil (lambda (c d)
		(if (<= target (+ i d))
		    (return-from roulette-selection c)
		    (incf i d)))
	  components probabilities)
    ;; only rarely should we ever get here
    (print 'RARITY)
    (first (last components))))


(defun select-component (problem solution alpha beta)
  "Selects a component for inclusion in a solution given a problem.  Returns NIL
if there is no valid component to add."
  ;; pick randomly among the valid components for the moment  -- FIXME
  (let ((v (valid-components problem solution)))
    (when v 
      (roulette-selection problem v alpha beta))))
       

(defun build-solution (problem alpha beta)
  "Iteratively builds a solution to a problem and returns it.  A solution
is a list of component indexes -- the earlier indexes were added later."
  (let (solution)
    (loop
       (let ((component (select-component problem solution alpha beta)))
	 (if component 
	     (push component solution) 
	    ;; (setf solution nil))))
	     (return))))
    solution))

(defun solution-fitness (problem solution)
  "Returns the fitness of a solution for a given problem."
  ;; We're using the sum total values for each component in the solution.
  (let ((sum 0))
    (dolist (c solution sum)
      (incf sum (aref (problem-values problem) c)))))


(defun update-pheromones (problem solutions fitnesses evaporation-rate)
  "Updates the pheromone values to reflect the provided solutions."
  (dotimes (i (length (problem-pheromones problem)))
    (setf (aref (problem-pheromones problem) i)
	  (* (- 1 evaporation-rate) 
	     (aref (problem-pheromones problem) i))))
  (map nil (lambda (s f)
	      (dotimes (i (length (problem-pheromones problem)))
		(if (member i s)
		    (incf (aref (problem-pheromones problem) i) f))))
	solutions fitnesses))

(defun as (problem &key (iterations 10000) (popsize 5) (alpha 1) (beta 5) (evaporation-rate 0.5) (initial-pheromones 50))
  "Each iteration, builds POPSIZE solutions, then updates the pheromones on those solutions.
Returns the best discovered solution."
  (let (best)
    (reset-problem problem initial-pheromones)
    (dotimes (i iterations (sort (copy-list best) #'<))
      (let ((solutions (fill-seq (make-list popsize) (lambda () (build-solution problem alpha beta)))))
	(let ((fitnesses (mapcar (lambda (s) (solution-fitness problem s)) solutions)))
	  (dolist (s solutions)
	    (if (or (null best) (< (solution-fitness problem best) (solution-fitness problem s)))
		(setf best s)))
	  (update-pheromones problem solutions fitnesses evaporation-rate)
					;(print-pheromones problem)
	  (format t "~%~a -> ~a" (solution-fitness problem best) (sort (copy-list best) #'<)))))))

(defvar *p* nil)
(with-open-file (s "knapsacks/100.in" :direction :input)
  (setf *p* (load-problem s)))
   
;(in-package :common-lisp)
