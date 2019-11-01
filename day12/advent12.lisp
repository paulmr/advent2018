(defpackage :advent12
  (:use :common-lisp))

(in-package :advent12)

(defparameter example "#..#.#..##......###...###")

(defun parse-initial-state (s) )

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
  (let ((fd (open fname)))
	(parse-initial-state (read-line fd))
	(read-line fd) ; blank after initial
	(loop
	   for line = (read-line fd nil)
	   while line
	   collect (parse-rule line)
	   finally (close fd))))

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

;; examples
(let
	((rules (make-rules (read-input "example.txt"))))

  (defun calc (initial-state n)
	(loop
	   repeat n
	   for gen from 0
	   for (offset . state) = (cons 0 initial-state) then (next-state rules state offset)
	   finally (return (trim-state state offset))))
  (calc (parse-state example) 20)
  ;;  (aref rules 9)
  ;; (print (format-state (next-state rules (parse-state example))))
  )
