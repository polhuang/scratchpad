;;; scratchpad.el --- Use the scratch buffer as a capture and edit space  -*- lexical-binding: t -*-

;; Copyright (c) 2023-2024 Paul Huang
;;
;; Author     : Paul Huang <paulleehuang.proton.me>
;; URL        : https://github.com/polhuang/scratchpad
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: December 31, 2023
;; Package-Version: 0.0.0
;; Keywords: scratch, capture, notes, note-taking, tools, buffer
;; Package-Requires: ((Emacs "26.1"))
;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projects (projectile or project.el), org-agenda and more.

;;; Code:

(require 'recentf)

;;
;;; Customization

(defgroup scratchpad nil
  "Scratch buffer with capture and edit."
  :group 'files
  :prefix "scratchpad-")
  
(defcustom scratchpad-buffer-name "*scratch*"
  "Custom scratchpad buffer name."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-save-file
  (expand-file-name "scratchpad.org" user-emacs-directory)
  "File storing scratchpad contents."
  :type 'file
  :group 'scratchpad)

(defcustom scratchpad-autosave-interval 120
  "The interval (in seconds) at which scratchpad will auto-save to file."
  :type 'number
  :group 'scratchpad)
 
(defcustom scratchpad-major-mode-initial
  (if (fboundp 'org-mode)
      'org-mode
    'lisp-interaction-mode)
  "The major mode scratchpad will use at initialization. Defaults to 'org mode' if installed; otherwise, defaults to 'lisp-interaction-mode'."
  :type 'function
  :group 'scratchpad)

;;
;;; Functions

;; add contents to save file as an org line item, then wipe
;; make persistent

;;;###autoload
(defun scratchpad-initialize ()
  "Format '*scratch*' buffer if already created."
  (setq initial-scratch-message nil)
  (scratchpad-restore-contents))

(defun scratchpad-other-window ()
  "Open the *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scratchpad-buffer-name)))

;;;###autoload
(defun scratchpad-restore-contents ()
  "Restore scratchpad file contents."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (insert-file-contents scratchpad-save-file)))

;;;###autoload
(defun scratchpad-toggle ()
  "Toggle scratchpad buffer."
  (interactive)
  (if (string= (buffer-name) scratchpad-buffer-name)
      (delete-window)
    (let ((selected-text (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end)))))
      (when selected-text
        (with-current-buffer (get-buffer-create scratchpad-buffer-name)
          (goto-char (point-max))
          (insert selected-text "\n"))))
    (scratchpad-other-window)))

(defun scratchpad-save-current ()
  "Save the contents of '*scratch*' buffer to `scratchpad-save-file` as a heading."
  (interactive)
  (let* ((scratch-buffer (get-buffer scratchpad-buffer-name))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (content (with-current-buffer scratch-buffer
                    (buffer-string))))
    (when (and scratch-buffer (not (string-empty-p content)))
      (with-temp-buffer
        (insert (concat "* " timestamp "\n" content "\n"))
        (append-to-file (point-min) (point-max) scratchpad-save-file))
      (with-current-buffer scratch-buffer
        (erase-buffer)))))

(provide 'scratchpad)
(scratchpad-initialize)
;;; scratchpad.el ends here
