;; quick-buffer-switch.el --- Quick switch to file or dir buffers.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-07-06
;; Last changed: 2012-05-21 17:46:46
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allows to qui switch to a buffer visiting a particular file or directory.
;;
;; To install:
;;   (require 'quick-buffer-switch)
;;   ;; To preserve C-x C-x uncomment next line
;;   ;; (setq qbs-prefix-key "C-x C-a")
;;   (qbs-init)
;;
;; Note that C-x C-c (`save-buffers-kill-terminal') is shadowed.


(eval-when-compile (require 'cl))
(eval-when-compile (require 'files))


(defvar quick-buffer-switch-map nil
  "Keymap for quick-buffer-switch commands.")

(defvar qbs-timeout 0.2
  "Timeout to when checking a path as directory.")

(defvar qbs-prefix-key "C-x C-c"
  "Prefix key used for `quick-buffer-switch-map'.

Note: By default it shadows `save-buffers-kill-terminal'.")

(defvar qbs-post-init-hook nil
  "Hook to be run after `qbs-init'.")

(defstruct (qbs:predicate :named)
  "Quick Buffer Switch predicate structure

 - name: the predicate name (read-only).
 - short-description: used when prompting.
 - description: long description used as function docstring.
 - timeout: override `qbs-timeout'.
 - pre-search: function called before running test on all buffers.
 - test: predicate test function.
 - post-search: function called after test run on all buffers.
 - shortcut: key binding used within `quick-buffer-switch-map'
   which prefix is C-x C-c.

Note that C-x C-c (`save-buffers-kill-terminal') is then shadowed.

The predicate function should take a buffer object as parameter,
and return a string which should be either a buffer name suitable
to `switch-to-buffer' or a path suitable to `find-file'."
  (name nil :read-only)
  short-description
  description
  timeout
  pre-search
  test
  post-search
  shortcut)

(defvar qbs-predicates-alist nil
  "List containing all predicates.

Do not modify directly, use `qbs-add-predicates' instead.")

(defun qbs-add-predicates (&rest predicates)
  "Add PREDICATES to `qbs-predicate-alist'."
  (loop for predicate in predicates
	do
	(unless (qbs:predicate-short-description predicate)
	       (setf (qbs:predicate-short-description predicate)
		     (save-match-data
		       (replace-regexp-in-string
			"-" " " (symbol-name (qbs:predicate-name predicate))))))
	     (aput 'qbs-predicates-alist (qbs:predicate-name predicate) predicate)

	     (let ((fname (format "qbs-%s" (qbs:predicate-name predicate)))
		   (doctring
		    (format "Quick switch buffer (%s predicate).\n\n%s"
			    (qbs:predicate-short-description predicate)
			    (or (qbs:predicate-description predicate)
				"No description available (see :description slot in `qbs-predicates-alist').")))
		   (key (qbs:predicate-shortcut predicate)))
	       (fset (intern fname)
		     `(lambda ()
			,doctring
			(interactive)
			(quick-buffer-switch (quote ,(qbs:predicate-name predicate)))))
	       (when key
		 (define-key quick-buffer-switch-map
		   (read-kbd-macro key) (intern fname))))))

;;;###autoload
(defun qbs-init ()
  "Initialize quick-buffer-switch."
  (qbs-add-predicates

   (make-qbs:predicate
    :name 'hidden-buffer
    :description "Show all hidden buffers (starting with a blank character)."
    :shortcut "C-h"
    :test '(when (string-match "^ " qbs:buffer-name)
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'matching-regexp
    :pre-search '(read-from-minibuffer "Search for (regexp): ")
    :shortcut "C-/"
    :test '(save-excursion
	     (save-restriction
	       (save-match-data
		 (goto-char (point-min))
		 (when
		     (search-forward-regexp qbs:pre-search nil t)
		   qbs:buffer-name)))))

   (make-qbs:predicate
    :name 'directory
    :shortcut "C-d"
    :test '(when (eq major-mode 'dired-mode)
	     (loop for d in dired-subdir-alist
		   append (list (abbreviate-file-name (car d))))))

   (make-qbs:predicate
    :name 'file-name
    :shortcut "C-f"
    :test '(let ((fname  (buffer-file-name)))
	    (when fname
	      (abbreviate-file-name fname))))

   (make-qbs:predicate
    :name 'file-buffer
    :shortcut "f"
    :test '(when qbs:buffer-file-name
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'file-or-directory
    :shortcut "C-c"
    :test '(if (eq major-mode 'dired-mode)
	       (loop for d in dired-subdir-alist
		     append (list (abbreviate-file-name (car d))))
	     (when qbs:buffer-file-name
	       qbs:buffer-file-name)))

   (make-qbs:predicate
    :name 'org-mode
    :shortcut "C-o"
    :test '(when (and qbs:buffer-file-name
   		      (eq major-mode 'org-mode))
   	     qbs:buffer-file-name))

   (make-qbs:predicate
    :name 'erc
    :shortcut "C-e"
    :test '(when (and
		  (eq major-mode 'erc-mode)
		  (not (get-buffer-process qbs:buffer-name)))
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'magit
    :shortcut "C-g"
    :test '(when (eq major-mode 'magit-mode) qbs:buffer-file-name))

   (make-qbs:predicate
    :name 'emacs-lisp-mode
    :shortcut "C-l"
    :test '(when (and qbs:buffer-file-name
   		      (eq major-mode 'emacs-lisp-mode))
   	     qbs:buffer-file-name))

   (make-qbs:predicate
    :name 'help-buffer
        :shortcut "C-i"
    :test '(when (or
		  (eq major-mode 'help-mode)
		  (eq major-mode 'Info-mode))
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'not-file-nor-directory
    :shortcut "C-b"
    :test '(unless (or
		    (eq major-mode 'dired-mode)
		    qbs:buffer-file-name
		    (string-match "^ " qbs:buffer-name))
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'with-process
    :shortcut "C-p"
    :test '(when (get-buffer-process qbs:buffer-name)
	     qbs:buffer-name))

   (make-qbs:predicate
    :name 'terminal
    :shortcut "C-v"
    :test '(when (or (eq major-mode 'shell-mode)
		     (eq major-mode 'term-mode)
		     qbs:buffer-name)))

   (make-qbs:predicate
    :name 'remote
    :shortcut "C-r"
    :test '(let* ((fname (or buffer-file-name
			     dired-directory))
		  (file-vec (or (ignore-errors (tramp-dissect-file-name
						fname))
				(tramp-dissect-file-name
				 (concat "/:" fname) 1)))
		  (host  (tramp-file-name-host file-vec)))
	     (when (and host
			(not (string= system-name host)))
	       (abbreviate-file-name fname))))

   ))

(defun qbs-get-buffer-names (predicate)
  "Return buffers matching PREDICATE.

PREDICATE should be a `qbs:predicate' object."
  (let ((qbs:pre-search (eval (qbs:predicate-pre-search predicate))))
    (loop for buffer in (buffer-list)
	  with bstr
	  do (with-timeout
		 ((or (qbs:predicate-timeout predicate) qbs-timeout)
		  (message (format "Timeout for %S" (buffer-name buffer))))
	       (with-current-buffer buffer
		 (let* ((qbs:buffer-name (buffer-name))
			(qbs:buffer-file-name
			 (when (buffer-file-name)
			   (abbreviate-file-name (buffer-file-name)))))
		   (setf bstr (eval (qbs:predicate-test predicate))))))
	  when (stringp bstr) collect bstr
	  when (listp bstr) append bstr)))


;;;###autoload
(defun qbs-find-buffer-visiting-dir (dir)
  "Find buffer visiting DIR. Return a maker or nil."
  (save-excursion
    (let ((dir (file-name-as-directory (expand-file-name dir))))
      (loop for buffer in (buffer-list)
	    with marker
	  do (with-timeout
		 (qbs-timeout
		  (message (format "Timeout for %S" (buffer-name buffer))))
	       (set-buffer buffer)
	       (when (eq major-mode 'dired-mode)
		 (let ((sub (assoc dir dired-subdir-alist)))
		   (when sub
		     (setf marker (cdr sub))))))
	  when marker
	  return marker))))

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
	 (predicate (cdr (assoc type qbs-predicates-alist)))
	 (blist (qbs-get-buffer-names predicate))
	 value)

    (if (not blist)
	(message (format "No buffer match '%s predicate" (qbs:predicate-name predicate)))
      (setq value (completing-read
		   (format "Switch to %s: "  (qbs:predicate-short-description predicate))
		   blist
		   nil t nil nil nil t))
      (cond
       ((file-directory-p value)
	(let ((mark (qbs-find-buffer-visiting-dir value)))
	  (when mark
	    (switch-to-buffer (marker-buffer mark))
	    (goto-char (marker-position mark)))))
       ((file-exists-p value)
	 (find-file value))
       (t
	(switch-to-buffer value))))))

(provide 'quick-buffer-switch)
