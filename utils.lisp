;;;; utils.lisp

(in-package #:utils)

(defmacro my-append (lst value)
  `(setf ,lst (nconc ,lst (list ,value))))

;curry macro; the assumption is that the n is less than
;the number of arguments expected by fn. Also, n + args
;should also be less than the number of arguments expected by fn
;expected by fn (note: not to be used; the cl-cookbook curry function below
;is superior)
(defmacro my-curry (fn n &rest args)
  (let ((x))
    (dotimes (i n)
      (setf x (nconc x (list (gensym)))))
    `(lambda (,@x) (,fn ,@args ,@x))))

;a simpler curry function from http://cl-cookbook.sourceforge.net/functions.html#curry
(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

;come up with a generic mapping function that
;maps a curried function to a list of lists,
;with the inner lists containing as many
;elements as are required as arguments for
;the curried function

;from http://norvig.com/paip/intro.lisp
(defun dot-product (a b)
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun flatten (lst)
  (let ((flattened-list))
    (dolist (x lst)
      (setq flattened-list (append flattened-list x)))
    flattened-list))

(defun random-interval (low high)
  (+ low (* (- high low) (random 1.0))))

;all permutations of a list.
;from http://forum.codecall.net/classes-code-snippets/15479-lisp-permutations.html
(defun all-permutations (lst)
  (if (null lst) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (all-permutations (remove x lst :count 1)))) lst)))

;all possible permutations of the elements of a list, taken n at a time
(defun permutations (lst n)
  (if (eq n 1)
      (loop for x in lst collect (list x))
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (permutations (remove x lst :count 1) (1- n)))) lst)))

;original version of permutations
(defun permutations-old (lst n)
  (if (eq n 1)
      (loop for x in lst collect (list x))
    (let ((result))
      (dolist (x lst)
        (dolist (y (permutations-old (remove x lst :count 1) (1- n)))
          (my-append result (append (list x) y))))
      result)))

;all possible combinations of the elements of a list, taken n at a time
(defun combinations (lst n)
  (labels ((same-list-p (lst1 lst2)
	     (and (eq (length lst1) (length lst2))
		  (all-elements-exist-p lst1 lst2)
		  (all-elements-exist-p lst2 lst1)))
           (my-and (lst)
                   (if (null lst) t
                     (and (not (null (car lst))) (my-and (cdr lst)))))
           (all-elements-exist-p (lst1 lst2)
                                 (my-and (loop for x in lst1 collect (find x lst2)))))
    (remove-duplicates (permutations lst n) :test #'same-list-p)))

;given a list of n elements, return a list
;of all possible combination of elements
;taken one at a time, two at a time, and so on
(defun all-combinations (lst)
  (loop for i from 1 to (length lst) collect (combinations lst i)))

(defun first-n (lst n)
  (let ((l (length lst)))
    (if (> l n) (butlast lst (- l n)) lst)))
