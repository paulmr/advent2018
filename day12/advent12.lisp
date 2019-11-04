(defpackage :advent12
  (:use :common-lisp))

(in-package :advent12)

(defparameter example "#..#.#..##......###...###")

(defun parse-initial-state (s)
  (parse-state (subseq s 15)))

(defun parse-rule (r)
  (let ((left (subseq r 0 5))
		(right (aref r 9)))
	(cons (bits-to-integer (parse-state left))
		  (char= #\# right))))

(defun trim-state (s offset)
  (flet ((skip (s)
		   (loop
			  for bit across s
			  for i from 0
			  if (= 1 bit) return i)))
	(let ((start (or (skip s) 0)) (end (- (length s) (skip (reverse s)))))
	  `(,(+ offset start) . ,(subseq s start end)))))

(defun read-input (fname)
  (let* ((fd (open fname))
		 (initial (parse-initial-state (read-line fd))))
	(read-line fd) ; blank after initial
	(loop
	   for line = (read-line fd nil)
	   while line
	   collect (parse-rule line) into rules
	   finally (close fd) (return (cons initial rules)))))

(defun next-state (rules s offset)
  (setq s (concatenate '(vector bit) #*0000 s #*0000))
  (setq offset (- offset 2))
;  (format t "calculating next state for: ~A~%~%" (format-state s))
  `(,offset .
	  ,(map '(vector bit)
			(lambda (p)
										; (format t "~A [~d] => ~d~%" (format-state p) (bits-to-integer p) (aref rules (bits-to-integer p)))
			  (aref rules (bits-to-integer p)))
			(split-state s))))

(defun make-rules (rules)
  (loop
	 with res = (make-array #x20 :element-type 'bit)
	 for (l . r) in rules
	 when r
	 do (setf (aref res l) 1)
	 finally (return res)))

(defun parse-state (s)
  (map '(vector bit) (lambda (n) (if (char= #\# n) 1 0)) s))

(defun format-state (s offset)
  ;; -3 is the window size that is used to display the output in the example
  (let ((bump (- offset -3)))
	(setq s
		  (if (> bump 0)
			  (concatenate '(vector bit) (make-array bump :element-type '(vector bit) :initial-element 0) s)
			  (subseq s (abs bump)))))
  (map 'string (lambda (b) (if (zerop b) #\. #\#)) s))

(defun bits-to-integer (bits)
  (loop
	 with res = 0
	 for b across bits
	 do (setq res (+ b (ash res 1)))
	   finally (return res)))

(defun split-state (s)
  (loop
	 for i from 0 to (- (length s) 5)
	 collect (subseq s i (+ 5 i))))
;; (let ((s (parse-state example)))
;;   (split-state (concatenate '(vector bit) #*00000 s #*00000)))

(defun sum-of-state (s offset)
  (loop
	 for b across s
	 for i from 0
	 when (= b 1)
	   sum (+ i offset)))

;; examples
(defun doit (fname count)
  (let* ((input (read-input fname))
		 (initial-state (car input))
		 (ruledesc (cdr input))
		 (rules (make-rules ruledesc))
		 (last-sum 0)
		 (last-diff 0)
		 (last-count 0))
	(loop
	   repeat (1+ count)
	   for gen from 0
	   for (offset . state) = (cons 0 initial-state)
	   then
		 (let ((next (next-state rules state offset)))
		   (trim-state (cdr next) (car next)))
	   for sum = (sum-of-state state offset)
	   for diff = (- sum last-sum)
	   if (= diff last-diff)
	   do
		 (incf last-count)
		 (if (> last-count 10)
			 ;; detected linear progression, so just extrapolate
			 (return (+ sum (* last-diff (- count gen)))))
	   else do (setq last-count 0)
	   do
		 (setq last-sum sum)
		 (setq last-diff diff)
	   finally (return (sum-of-state state offset)))))

  ;; (let
  ;; 	  (((initial . rules) ))

  ;; 	(defun calc (initial-state n)
  ;; 	  (loop
  ;; 		 repeat n
  ;; 		 for gen from 0

  ;; 	(let ((res (calc (parse-state example) 21)))
  ;; 	  (sum-of-state (cdr res) (car res))
  ;; 	  )
  ;; 	;;  (aref rules 9)
  ;; 	;; (print (format-state (next-state rules (parse-state example))))
  ;; 	)

;; (doit "/sdcard/lisp/example.txt" 20)

(format t "Part 1: ~d~%Part 2: ~d~%"
		(doit "/sdcard/lisp/input12.txt" 20)
		(doit "/sdcard/lisp/input12.txt" 50000000000))
