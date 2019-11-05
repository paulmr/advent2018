#!/usr/bin/env -S cl -E advent12:main
(defpackage :advent12
  (:export :main)
  (:use :common-lisp))

(in-package :advent12)

(defun read-input (fname)
  (let* ((fd (open fname))
		 (initial (subseq (read-line fd) 15)))
	(read-line fd) ; blank after initial
	(loop
	   for line = (read-line fd nil)
	   while line
	   when (char= (aref line 9) #\#)
	   collect (subseq line 0 5) into rules
	   finally (close fd) (return (cons initial rules)))))

(defun next-state (rules s offset)
  (cons
   (- offset 2)
   (map
	'string
	(lambda (p) (if (find p rules :test #'equal) #\# #\.))
	(split-state (concatenate 'string "...." s "....")))))

(defun split-state (s)
  (loop
	 for i from 0 to (- (length s) 5)
	 collect (subseq s i (+ 5 i))))

(defun sum-of-state (s offset)
  (loop
	 for pot across s
	 for num from 0
	 when (char= pot #\#)
	 sum (+ num offset)))

(defun calc (initial-state rules count)
  (let ((last-sum 0)
		(last-diff 0)
		(last-count 0))
	(loop
	   repeat (1+ count)
	   for gen from 0
	   for (offset . state) = (cons 0 initial-state)
	   then (next-state rules state offset)
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

(defun main (argv)
  (let* ((fname (or (car argv) "input12.txt"))
		 (input (read-input fname))
		 (initial-state (car input))
		 (rules (cdr input)))
	(format t "Part 1: ~d~%" (calc initial-state rules 20))
	(format t "Part 2: ~d~%" (calc initial-state rules 50000000000))))
