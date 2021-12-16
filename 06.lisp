(let ((counts (make-hash-table :test 'equal)))
  (defun count-after-cycles (n c)
    (if (= n 0)
      1
      (if (not (null (gethash (format nil "~d ~d" n c) counts)))
	(gethash (format nil "~d ~d" n c) counts)
	(if (= c 0)
	  (setf (gethash (format nil "~d ~d" n c) counts) (+ (count-after-cycles (- n 1) 6) (count-after-cycles (- n 1) 8)))
	  (setf (gethash (format nil "~d ~d" n c) counts) (count-after-cycles (- n 1) (- c 1))))))
    ))

(defun counts-after-cycles (n counts)
  (reduce (lambda (accum counter)
	    (+ accum (count-after-cycles n counter)))
	  counts
	  :initial-value 0))

(defparameter *input*
  '(1 4 1 1 1 1 1 1 1 4 3 1 1 3 5 1 5 3 2 1 1 2 3 1 1 5 3 1 5 1 1 2 1 2 1 1 3 1 5 1 1 1 3 1 1 1 1 1 1 4 5 3 1 1 1 1 1 1 2 1 1 1 1 4 4 4 1 1 1 1 5 1 2 4 1 1 4 1 2 1 1 1 2 1 5 1 1 1 3 4 1 1 1 3 2 1 1 1 4 1 1 1 5 1 1 4 1 1 2 1 4 1 1 1 3 1 1 1 1 1 3 1 3 1 1 2 1 4 1 1 1 1 3 1 1 1 1 1 1 2 1 3 1 1 1 1 4 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 5 1 1 1 2 2 1 1 3 5 1 1 1 1 3 1 3 3 1 1 1 1 3 5 2 1 1 1 1 5 1 1 1 1 1 1 1 2 1 2 1 1 1 2 1 1 1 1 1 2 1 1 1 1 1 5 1 4 3 3 1 3 4 1 1 1 1 1 1 1 1 1 1 4 3 5 1 1 1 1 1 1 1 1 1 1 1 1 1 5 2 1 4 1 1 1 1 1 1 1 1 1 1 1 1 1 5 1 1 1 1 1 1 1 1 2 1 4 4 1 1 1 1 1 1 1 5 1 1 2 5 1 1 4 1 3 1 1))

(defun solve ()
  (counts-after-cycles 256 *input*))
