;; quick-buffer-switch.el --- Quick switch to file or dir buffers.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-07-06
;; Last changed: 2011-07-07 12:53:14
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allows to qui switch to a buffer visiting a particular file or directory.
;;
;; To install:
;;   (require 'quick-buffer-switch)
;;
;; Then following bindings are available:
;;   C-x C-c C-c    `qbs-files-or-directories'
;;   C-x C-c C-d    `qbs-files-directories'
;;   C-x C-c C-f    `qbs-files'
;;
;; Note that C-x C-c (`save-buffers-kill-terminal') is then shadowed.


(eval-when-compile (require 'cl))
(eval-when-compile (require 'files))


(defvar quick-buffer-switch-map nil
  "Keymap for quick-buffer-switch commands.")

(define-prefix-command 'quick-buffer-switch-map)
(global-set-key (kbd "C-x C-c") 'quick-buffer-switch-map)
(define-key quick-buffer-switch-map (kbd "C-c") 'qbs-files-or-directories)
(define-key quick-buffer-switch-map (kbd "C-d") 'qbs-directories)
(define-key quick-buffer-switch-map (kbd "C-f") 'qbs-files)


(defvar qbs-files-or-directories-history '()
  "History list for files or directories switch.")

(defvar qbs-directories-history '()
  "History list for directories switch.")

(defvar qbs-files-history '()
  "History list for files switch.")

(defconst qbs-dispatcher
  '((f-o-d . ("" qbs-get-visited-files-or-directories-list qbs-files-or-directories-history))
    (d . (" (dir)" qbs-get-visited-directories-list  qbs-directories-history))
    (f . (" (file)" qbs-get-visited-files-list  qbs-files-history)))
  "Dispatcher for quick-buffer-switch. Format is:

    \( type . \( description function history \)\).
")

(defun qbs-get-visited-files-or-directories-list()
  "Return a list of all files or directories visited by a buffer."
  (loop for buffer in (buffer-list)
	with bname 
	do (progn
	     (set-buffer buffer)
	     (setq bname (if (eq major-mode 'dired-mode)
			     default-directory
			   (buffer-file-name))))
	when bname collect (abbreviate-file-name bname)))

(defun qbs-get-visited-directories-list()
  "Return a list of all directories visited by a buffer."
  (loop for buffer in (qbs-get-visited-files-or-directories-list)
	when (file-directory-p buffer)
	collect buffer))

(defun qbs-get-visited-files-list()
  "Return a list of all files visited by a buffer."
  (loop for buffer in (qbs-get-visited-files-or-directories-list)
	unless (file-directory-p buffer)
	collect buffer))

(defun quick-buffer-switch (type &optional filename)
  "Switch to buffer visiting FILENAME or prompt according TYPE:
file or directory (f-o-d), directory (d) or file (f)."
  (save-current-buffer
    (let* ((conf (assoc type qbs-dispatcher))
	   (desc (cadr conf))
	   (func (caddr conf))
	   (hist (cadddr conf))
	   (filename (or filename
			 (completing-read
			  (format "Switch to buffer visiting%s: " desc)
			  (funcall func)
			  nil t nil hist nil t))))
      (when filename
	(find-file filename)))))

;;;###autoload
(defun qbs-files-or-directories (&optional filename)
  "Switch to buffer visiting FILENAME or prompt or a file a
directory."
  (interactive)
  (quick-buffer-switch 'f-o-d filename))

;;;###autoload
(defun qbs-directories (&optional filename)
  "Switch to buffer visiting FILENAME or prompt or a directory."
  (interactive)
  (quick-buffer-switch 'd filename))

;;;###autoload
(defun qbs-files (&optional filename)
  "Switch to buffer visiting FILENAME or prompt or a file."
  (interactive)
  (quick-buffer-switch 'f filename))

