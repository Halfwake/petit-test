;; The MIT License (MIT)
;;
;; Copyright (c) 2015 Andrew "Drew" Dudash
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.



;;;; petit-test: less features means less code

;; This is what defining a test looks like.
;; (define-test test-+
;;   (test (= 3 (+ 1 2)))
;;   (test (= 4 (+ 1 2)))
;;   (test (= 5 (+ 3 2))))
;;
;; This is what running a test looks like.
;; CL-USER> (test-+)
;; pass (= 3 (+ 1 2))
;; FAIL (= 4 (+ 1 2))
;; pass (= 5 (+ 3 2))
;; TEST-+ Passed: 2/3 Failed: 1/3


(in-package :petit-test)

(defpackage :petit-test
  (:use :common-lisp)
  (:export :test
	   :define-test))

(defun print-test-message (success expression)
  "Prints result of a single test."
  (format t "~:[FAIL~;pass~] ~a~%" success expression)
  success)

(defun print-test-results (test-name results)
  "Prints results of test."
  (loop for result in results
     counting result into pass-count
     counting (not result) into fail-count
     counting t into total-count
     finally (format t "~a Passed: ~a/~a Failed: ~a/~a~%"
		     test-name pass-count total-count fail-count total-count)))

(defmacro test (test-expr)
  "Prints the result of test and returns the result."
  `(print-test-message ,test-expr ',test-expr))

(defmacro define-test (test-name &body clauses)
  "Every form inside of a define-test is a test. Tests pass if they return true."
  `(defun ,test-name ()
     (print-test-results ',test-name (list ,@(loop for clause in clauses collect
						  `(test ,clause))))))
