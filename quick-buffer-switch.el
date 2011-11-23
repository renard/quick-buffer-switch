;; quick-buffer-switch.el --- Quick switch to file or dir buffers.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-07-06
;; Last changed: 2011-11-23 15:12:15
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allows to qui switch to a buffer visiting a particular file or directory.
;;
;; To install:
;;   (require 'quick-buffer-switch)
;;   (qbs-init)
;;
;; Note that C-x C-c (`save-buffers-kill-terminal') is shadowed.


(eval-when-compile (require 'cl))
(eval-when-compile (require 'files))


(defvar quick-buffer-switch-map nil
`  "Keymap for quick-buffer-switch commands.")

(defvar qbs-timeout 0.2
  "Timeout to when checking a path as directory.")

(defvar
  qbs-predicates-alist
  '((hidden-buffer "hidden buffer"
		   (lambda (b)
		     (let ((bname (buffer-name b)))
		       (when (string-match "^ " bname)
			 bname)))
		   "C-h")
    (directory "directory"
	       (lambda (b)
		 (when (eq major-mode 'dired-mode)
		   (abbreviate-file-name default-directory)))
	       "C-d")
     (file "file"
	   (lambda (b)
	     (let ((fname (buffer-file-name)))
	       (when fname
		 (abbreviate-file-name fname))))
	   "C-f")
     (org-file "org file"
	       (lambda (b)
		 (let ((fname (buffer-file-name)))
	       (when (and
		      fname
		      (eq major-mode 'org-mode))
		 (abbreviate-file-name fname))))
	       "C-o")
     (file-or-directory "file or directory"
			(lambda (b)
			  (let ((fname (if (eq major-mode 'dired-mode)
					   default-directory
					 (buffer-file-name))))
			    (when fname
			      (abbreviate-file-name fname))))
			"C-c")
     (erc-chat "ERC chat"
	       (lambda (b)
		 (when (and
			(eq major-mode 'erc-mode)
			(not (get-buffer-process b)))
		   (buffer-name b)))
	       "C-e")
     (help "help buffer"
	   (lambda (b)
		 (when (or
			(eq major-mode 'help-mode)
			(eq major-mode 'Info-mode))
		   (buffer-name b)))
	   "C-i")
     (not-file-nor-directory "not file related buffer"
			     (lambda (b)
			       (let ((bname (buffer-name b)))
				 (unless (or
					  (eq major-mode 'dired-mode)
					  (buffer-file-name)
					  (string-match "^ " bname))
				   bname)))
			     "C-b")
     (with-process "buffer with process"
		   (lambda (b)
		     (let* ((bname (buffer-name b)))
		       (when (get-buffer-process b)
			 bname)))
		   "C-p")
     )
  "List of `quick-buffer-switch' predicate. Each predicate consists of:

- A symbol used by `qbs-get-buffer-names' and to create a
  qbs-SYMBOL function.
- A string definition used as prompt.
- A 1-parameter predicate function
- An optional key binding used within `quick-buffer-switch-map'
  which prefix is C-x C-x.

Note that C-x C-c (`save-buffers-kill-terminal') is then shadowed.

The predicate function should take a buffer object as parameter,
and return a string which should be either a buffer name suitable
to `switch-to-buffer' or a path suitable to `find-file'.")

;;;###autoload
(defun qbs-init ()
  "Initialize quick-buffer-switch."
  (define-prefix-command 'quick-buffer-switch-map)
  (global-set-key (kbd "C-x C-c") 'quick-buffer-switch-map)
  (dolist (type  (mapcar 'car qbs-predicates-alist))
    (let* ((predicate-def (assoc type qbs-predicates-alist))
	   (key (cadddr predicate-def))
	   (fname (concat "qbs-" (symbol-name type))))
      (fset (intern fname)
	    `(lambda ()
	       "Quick switch buffer."
	       (interactive)
	       (quick-buffer-switch (quote ,type))))
      (message "Key: %S" key)
      (when key
	(define-key quick-buffer-switch-map (read-kbd-macro key) (intern fname))))))

(defun qbs-get-buffer-names (predicate)
  "Return buffers matching PREDICATE.

PREDICATE should be a sexp with a BUFFER parameter and return a
string representation of the buffer which would be used in `completing-read'."
  (loop for buffer in (buffer-list)
	with bstr
	do (with-timeout
	       (qbs-timeout
		(message (format "Timeout for %S" (buffer-name buffer))))
	     (set-buffer buffer)
	     (setq bstr (funcall predicate buffer)))
	when bstr collect bstr))

;;;###autoload
(defun quick-buffer-switch (&optional type)
  "Quick switch buffer switch according TYPE. Seed `qbs-predicates-alist'."
  (interactive)
  (let* ((type (or type
		   (intern (completing-read
			    "Quick buffer switch predicate: "
			    (loop for p in qbs-predicates-alist
				  collect (symbol-name (car p)))
			    nil t nil nil nil t))))
	 (predicate-def (assoc type qbs-predicates-alist))
	 (msg (cadr predicate-def))
	 (predicate (caddr predicate-def))
	 (value (completing-read
		 (format "Switch to %s: " msg)
		 (qbs-get-buffer-names predicate)
		 nil t nil nil nil t)))
    (cond
     ((or (file-exists-p value)
	  (file-directory-p value))
      (find-file value))
     (t
      (switch-to-buffer value)))))

(provide 'quick-buffer-switch)
