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
  (let* ((session (or (get-buffer "*inferior-lisp*")
		      (save-window-excursion
			(run-lisp ob-acl2-program)
			(sleep-for .5)
			(current-buffer))))
	 (valuep (eq 'value (cdr (assoc :result-type params))))
	 (wrapped-body (if valuep (format "(format nil \"~a~n\" %s)" body) body))
	 (full-body (org-babel-expand-body:emacs-lisp wrapped-body params))
	 (lines
	  (org-babel-comint-with-output (session ob-acl2-eoe-indicator)
	    (setq-local comint-prompt-regexp "^ACL2 !> *")
	    (insert full-body)
	    (comint-send-input nil t)
	    (insert ob-acl2-eoe-indicator)
	    (comint-send-input nil t))))
    (cl-loop for line in lines
	     for trimmed = (substring line 0 (string-match ob-acl2-eoe-indicator line))
	     collect trimmed into results
	     finally return (mapconcat 'identity results "\n"))))


(provide 'ob-acl2)
;;; ob-acl2.el ends here
