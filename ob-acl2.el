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
(require 'org)
(require 'ob)
(require 'ob-comint)


(defgroup ob-acl2 nil
  "Customization group for `ob-acl2'."
  :group 'org-babel
  :prefix 'ob-acl2)


(defcustom ob-acl2-program "acl2"
  "The ACL2 program to use."
  :group 'ob-acl2
  :type 'string)


(defvar org-babel-acl2-eoe-indicator "\"ACL2-EOE\""
  "String that signals the end of an ACL2 evaluation.")


;;;###autoload
(defun org-babel-execute:acl2 (body params)
  "Execute a block of ACL2 code with org-babel."
  (message "executing ACL2 source code block")  
  (let* ((processed-params (org-babel-process-params params))
	 (session (org-babel-acl2-initiate-session (cdr (assoc :session processed-params))))
	 (full-body (org-babel-expand-body:acl2 body params))	 
	 (lines
	  (org-babel-comint-with-output (session org-babel-acl2-eoe-indicator)
	    (setq comint-prompt-regexp "^ACL2 !> *")
	    (insert (org-trim full-body))
	    (comint-send-input nil t)
	    (insert org-babel-acl2-eoe-indicator)
	    (comint-send-input nil t))))
    (cl-loop for line in lines
	     for trimmed = (substring line 0 (string-match org-babel-acl2-eoe-indicator line))
	     collect trimmed into results
	     finally return (mapconcat 'identity results "\n"))))


;;;###autoload
(defun org-babel-expand-body:acl2 (body params)
  "Expand BODY according to RESULT-TYPE."
  (org-babel-expand-body:emacs-lisp
   (if (eq 'value (cdr (assoc :result-type params)))
       (format "((lambda () %s))" body)
     body)
   params))



;;;###autoload
(defun org-babel-acl2-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create one."
  (or (get-buffer "*inferior-lisp*")
      (save-window-excursion
	(run-lisp ob-acl2-program)
	(sleep-for .5)
	(current-buffer))))


;;;###autoload
(defun org-babel-acl2-evaluate (session body)
  "Evaluate BODY in SESSION."
  (org-babel-comint-with-output (session org-babel-acl2-eoe-indicator body)
    (mapc
     (lambda (line)
       (insert (org-babel-chomp line))
       (comint-send-string nil t)
       (comint-send-string nil "\n"))
     (list (org-trim body) org-babel-acl2-eoe-indicator))
    (insert body)
    (comint-send-string nil "\n")))


;;;###autoload
(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("acl2" . lisp)))


(provide 'ob-acl2)
;;; ob-acl2.el ends here
