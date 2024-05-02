;;; ob-acl2-test.el --- Test of Babel Functions for ACL2    -*- lexical-binding: t; -*-
;;;
;;; Copyright (C) 2024 TANIGUCHI Masaya
;;;
;;; Author: TANIGUCHI Masaya <masaya.taniguchi@a.riken.jp>
;;; Version: 3.1
;;; Homepage: https://github.com/tani/ob-acl2
;;; Package-Requires: ((emacs "28") (org "9"))
;;; Keywords: tools, org, literate programming, theorem proving, ACL2, proof assistant system
;;; License: GPL-3.0-or-later
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; Commentary:
;;;
;;; This file contains the tests for the functions in ob-acl2.el.
;;; 
;;; Code:

(add-to-list 'load-path (expand-file-name "."))
(require 'ert)
(require 'ob-acl2)


(ert-deftest ob-acl2-test--ob-acl2-clean-up-output ()
  (let ((output-lines '("result line 1" "result ilne 2" "\"ACL2-EOE\"")))
    (should (equal (ob-acl2-clean-up-output output-lines)
		   "result line 1\nresult ilne 2"))))


(ert-deftest ob-acl2-test--ob-acl2-expand-body ()
  (let ((params '((:result-type . value))))
    (should (equal
	     (car (read-from-string (ob-acl2-expand-body "(+ 1 1)" params)))
	     '((lambda () (+ 1 1))))))
  (let ((params '((:result-type . output))))
    (should (equal
	     (car (read-from-string (ob-acl2-expand-body "(+ 1 1)" params)))
	     '(+ 1 1))))
  (let ((params '((:result-type . value) (:var x . 1) (:var y . 2))))
    (should (equal
	     (car (read-from-string (ob-acl2-expand-body "(+ x y)" params)))
	     '(let ((x '1) (y '2)) ((lambda () (+ x y)))))))
  (let ((params `((:result-type . output) (:var x . 1) (:var y . 2))))
    (should (equal
	     (car (read-from-string (ob-acl2-expand-body "(+ x y)" params)))
	     '(let ((x '1) (y '2)) (+ x y))))))


(ert-deftest ob-acl2-test--ob-acl2-initiate-session ()
  (let ((session "test-session")
	(kill-buffer-query-functions nil))
    (kill-buffer (get-buffer-create (format "*inferior-lisp<ob-acl2:%s>*" session)))
    (should (equal (buffer-name (ob-acl2-initiate-session session))
		   (format "*inferior-lisp<ob-acl2:%s>*" session)))
    (kill-buffer (get-buffer-create (format "*inferior-lisp<ob-acl2:%s>*" session)))))


(ert-deftest ob-acl2-test--ob-ac2-evaluate-body ()
  (let* ((kill-buffer-query-functions nil)
	 (session (ob-acl2-initiate-session "test-session"))
	 (full-body "(+ 1 1)"))
    (kill-buffer (get-buffer-create (format "*inferior-lisp<ob-acl2:%s>*" session)))
    (should (equal (ob-acl2-evaluate-body full-body session) '("2\n" "\"ACL2-EOE\"\n")))
    (kill-buffer (get-buffer-create (format "*inferior-lisp<ob-acl2:%s>*" session)))))


(provide 'ob-acl2-test)
;;; ob-acl2-test.el ends here
