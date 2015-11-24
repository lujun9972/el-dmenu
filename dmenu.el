;;; dmenu.el --- use ido to simulate the dmenu command line program

;; Copyright (C) 2009-2014 Cornelius Mika and contributors
;;
;; Author: Cornelius Mika <cornelius.mika@gmail.com> and contributors
;; URL: http://github.com/lujun9972/el-dmenu/
;; Package-Requires: ((emacs "24"))
;; Version: 0.1
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;; run (dmenu-initialize)
;;
;; Bind the following commands:
;; dmenu
;;
;; For a detailed introduction see:
;; http://github.com/lujun9972/el-dmenu/blob/master/README.org

;;; Code:

(require 'ido)
(require 'cl)
(defgroup dmenu nil
  "M-x interface with Ido-style fuzzy matching and ranking heuristics."
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "dmenu.el"))

(defcustom dmenu-save-file (locate-user-emacs-file "dmenu-items")
  "File in which the dmenu state is saved between Emacs sessions.
Variables stored are: `dmenu--cache-executable-files', `dmenu--history-list'.
Must be set before initializing Dmenu."
  :type 'string
  :group 'dmenu)

(defcustom dmenu-prompt-string ": "
  "String to display in the dmenu prompt."
  :type 'string
  :group 'dmenu)

(defcustom dmenu-flex-matching t
  "Enables Ido flex matching. On by default.
Set this to nil to disable fuzzy matching."
  :type 'boolean
  :group 'dmenu)

(defcustom dmenu-history-size 7
  "Determines on how many recently executed commands dmenu should keep a record. "
  :type 'integer
  :group 'dmenu)

(defvar dmenu-initialized-p nil)

(defvar dmenu--history-list nil)

(defvar dmenu--cache-executable-files nil)

;;;###autoload
(defun dmenu(&optional prefix)
  (interactive "p")
  (unless dmenu-initialized-p
	(dmenu-initialize))
  (unless dmenu--cache-executable-files
	(dmenu--cache-executable-files))
  (let* ((ido-enable-flex-matching dmenu-flex-matching)
		 (execute-file (ido-completing-read+ dmenu-prompt-string
											 (append dmenu--history-list
													 (remove-if (lambda (x)
																  (member x dmenu--history-list))
																dmenu--cache-executable-files))
											 nil
											 'confirm
											 nil
											 'dmenu--history-list))
		 args)
	(when (= prefix 4)
	  (setq args (read-string "please input the parameters: "))
	  (with-temp-buffer
	  	(insert args)
	  	(setq args (car (shell--parse-pcomplete-arguments)))))
	(setq dmenu--history-list (cons execute-file (remove execute-file dmenu--history-list)))
	(when (> (length dmenu--history-list) dmenu-history-size)
	  (setcdr (nthcdr (- dmenu-history-size 1) dmenu--history-list) nil))
	(switch-to-buffer (apply #'make-comint execute-file execute-file nil args))))

;;;###autoload
(defun dmenu-initialize ()
  (interactive)
  (unless ido-mode (dmenu-initialize-ido))
  (dmenu-load-save-file)
  (dmenu-auto-update)
  (add-hook 'kill-emacs-hook 'dmenu-save-to-file)
  (setq dmenu-initialized-p t))

(defun dmenu-initialize-ido ()
  "Sets up a minimal Ido environment for `ido-completing-read'."
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup))


(defun dmenu-load-save-file ()
  "Loads `dmenu--history-list' and `dmenu--cache-executable-files' from `dmenu-save-file'"
  (let ((save-file (expand-file-name dmenu-save-file)))
    (if (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
		  (ignore-errors
			(setq dmenu--cache-executable-files (read (current-buffer)))
			(setq dmenu--history-list (read (current-buffer)))))
      (setq dmenu--history-list nil
			dmenu--cache-executable-files nil))))

(defun dmenu-save-to-file ()
  "Saves `dmenu--history-list' and `dmenu--cache-executable-files' to `dmenu-save-file'"
  (interactive)
  (with-temp-file (expand-file-name dmenu-save-file)
    (ido-pp 'dmenu--cache-executable-files)
	(ido-pp 'dmenu--history-list)))


(defun dmenu--cache-executable-files()
  "cache executable files"
  (let* ((valid-exec-path (remove-if-not #'file-exists-p (remove-if-not #'stringp exec-path)))
		 (files (mapcan (lambda (dir)
						  (directory-files dir t nil nil)) valid-exec-path))
		 (executable-files (mapcar #'file-name-nondirectory (remove-if #'file-directory-p (remove-if-not #'file-executable-p files)))))
	(setq dmenu--cache-executable-files (sort executable-files #'string<))))

(defvar dmenu--update-timer nil)

(defun dmenu-auto-update (&optional idle-time)
  "Update dmenu when Emacs has been idle for IDLE-TIME."
  (let ((idle-time (or idle-time 60)))
	(when dmenu--update-timer
	  (cancel-timer dmenu--update-timer))
	(setq dmenu--update-timer (run-with-idle-timer idle-time t #'dmenu--cache-executable-files))))

(provide 'dmenu)

;;; dmenu.el ends here
