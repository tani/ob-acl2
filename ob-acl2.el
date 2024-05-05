;;; ob-acl2.el --- Babel Functions for ACL2    -*- lexical-binding: t; -*-
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
;;; This file contains the org-babel functions for ACL2.
;;; 
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ob-comint)

(defgroup ob-acl2 nil
  "Customization group for `ob-acl2'."
  :group 'org-babel
  :prefix 'ob-acl2)

(defcustom ob-acl2-program "acl2"
  "The ACL2 program to use."
  :group 'ob-acl2
  :type 'string)

(defvar ob-acl2-eoe-indicator "\"ACL2-EOE\""
  "String that signals the end of an ACL2 evaluation.")

;;;###autoload
(defun org-babel-execute:acl2 (body params)
  "Execute BODY with PARAMS in ACL2."
  (message "executing ACL2 source code block")
  (let* ((full-body (org-babel-expand-body:acl2 body params))
	 (session (ob-acl2-initiate-session (alist-get :session params)))
	 (output-lines (ob-acl2-evaluate-body full-body session)))
    (ob-acl2-clean-up-output output-lines)))

;;;###autoload
(defun org-babel-expand-body:acl2 (body params)
  "Expand BODY with PARAMS as the same as an emacs-lisp source code block."
  (org-babel-expand-body:emacs-lisp
   (if (eq 'value (alist-get :result-type params))
       (format "((lambda () %s))" body)
     body)
   params))

;;; Helper functions

(defun ob-acl2-clean-up-output (output-lines)
  "Clean up OUTPUT-LINES by removing the ACL2-EOE indicator."
  (cl-loop for line in output-lines
	   until (string-match-p ob-acl2-eoe-indicator line)
	   collect line into cleaned-lines
	   finally return (string-join cleaned-lines "\n")))

(defun ob-acl2-evaluate-body (full-body session)
  "Evaluate FULL-BODY in SESSION."
  (org-babel-comint-with-output (session ob-acl2-eoe-indicator)
    (setq-local comint-prompt-regexp "^ACL2 !> *")
    (insert full-body)
    (comint-send-input nil t)
    (insert ob-acl2-eoe-indicator)
    (comint-send-input nil t)))

(defun ob-acl2-initiate-session (session)
  "Initiate an ACL2 session named SESSION."
  (let* ((session-name (format "*inferior-lisp<ob-acl2:%s>*" session))
	 (buffer (get-buffer session-name)))
    (unless buffer
      (save-window-excursion
	(run-lisp ob-acl2-program)
	(rename-buffer session-name)
	(setq buffer (current-buffer))
	(ob-acl2-evaluate-body "\"ACL2-INIT\"" buffer)))
    buffer))

(provide 'ob-acl2)
;;; ob-acl2.el ends here
