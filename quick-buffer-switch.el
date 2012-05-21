;; quick-buffer-switch.el --- Quick switch to file or dir buffers.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-07-06
;; Last changed: 2012-05-16 00:01:24
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

 - name: the predicate name (read-only)."
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

Do not modify directly, use `qbs-add-predicate' instead.")

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
		 (loop for d in dired-subdir-alist
		       append (list (abbreviate-file-name (car d)))))
	       "C-d")
     (file-name "file name"
	   (lambda (b)
	     (let ((fname (buffer-file-name)))
	       (when fname
		 (abbreviate-file-name fname))))
	   "C-f")
     (file-buffer "file buffer"
	   (lambda (b)
	     (let ((fname (buffer-file-name)))
	       (when fname
		 (buffer-name b))))
	   "f")
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
			  (if (eq major-mode 'dired-mode)
			      (loop for d in dired-subdir-alist
				    append (list (abbreviate-file-name (car d))))
			    (let ((fname (buffer-file-name)))
			      (when fname
				(abbreviate-file-name fname)))))
			"C-c")
     (erc-chat "ERC chat"
	       (lambda (b)
		 (when (and
			(eq major-mode 'erc-mode)
			(not (get-buffer-process b)))
		   (buffer-name b)))
	       "C-e")
     (magit-mode "Magit buffers"
	       (lambda (b)
		 (when (eq major-mode 'magit-mode)
		   (buffer-name b)))
	       "C-g")
     (emacs-lisp-mode "Emacs Lisp buffers"
	       (lambda (b)
		 (when (eq major-mode 'emacs-lisp-mode)
		   (buffer-name b)))
	       "C-l")
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
     (remote "remote buffer"
	     (lambda (b)
	       (let* ((bname (buffer-name b))
		      (fname (or buffer-file-name
				dired-directory))
		      (file-vec (or (ignore-errors (tramp-dissect-file-name
						    fname))
				    (tramp-dissect-file-name
				     (concat "/:" fname) 1)))
		      (host  (tramp-file-name-host file-vec)))
		 (message (format "Format %S" host))
		 (when (and host
			    (not (string= system-name host)))
		   (abbreviate-file-name fname))))
	     "C-r")
     (term "terminal"
	   (lambda (b)
		 (when (or
			(eq major-mode 'shell-mode)
			(eq major-mode 'term-mode))
		   (buffer-name b)))
	   "C-v")
     )
  "List of `quick-buffer-switch' predicate. Each predicate consists of:

- A symbol used by `qbs-get-buffer-names' and to create a
  qbs-SYMBOL function.
- A string definition used as prompt.
- A 1-parameter predicate function
- An optional key binding used within `quick-buffer-switch-map'
  which prefix is C-x C-c.

Note that C-x C-c (`save-buffers-kill-terminal') is then shadowed.

The predicate function should take a buffer object as parameter,
and return a string which should be either a buffer name suitable
to `switch-to-buffer' or a path suitable to `find-file'.")

;;;###autoload
(defun qbs-init ()
  "Initialize quick-buffer-switch."
  (define-prefix-command 'quick-buffer-switch-map)
  (global-set-key (read-kbd-macro qbs-prefix-key) 'quick-buffer-switch-map)
  (define-key quick-buffer-switch-map (kbd "?") 'quick-buffer-switch)
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
	(define-key quick-buffer-switch-map (read-kbd-macro key) (intern fname)))))
  (run-hooks 'qbs-post-init-hook))

(defun qbs-get-buffer-names (predicate)
  "Return buffers matching PREDICATE.

PREDICATE should be a sexp with a BUFFER parameter and return a
string representation of the buffer which would be used in `completing-read'."
  (loop for buffer in (buffer-list)
	with bstr
	do (with-timeout
	       (qbs-timeout
		(message (format "Timeout for %S" (buffer-name buffer))))
	     (with-current-buffer buffer
	       (setf bstr (funcall predicate buffer))))
	when (stringp bstr) collect bstr
	when (listp bstr) append bstr))


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
	 (predicate-def (assoc type qbs-predicates-alist))
	 (msg (cadr predicate-def))
	 (predicate (caddr predicate-def))
	 (blist (qbs-get-buffer-names predicate))
	 value)


    (if (not blist)
	(message (format "No buffer match '%s predicate" type))
      (setq value (completing-read
		   (format "Switch to %s: " msg)
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
