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
(require 'transient)

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
  "Initial major mode for scratchpad. Defaults to `org-mode' if installed;
otherwise, defaults to `lisp-interaction-mode'."
  :type 'function
  :group 'scratchpad)

(defcustom scratchpad-menu-key "C->"
  "Key binding to activate the scratchpad menu in the scratch buffer."
  :type 'string
  :group 'scratchpad)

;;
;;; Functions

;;;###autoload
(defun scratchpad-initialize ()
  "Format '*scratch*' buffer if already created."
  (setq initial-scratch-message nil)
  (scratchpad-restore-contents)
  (with-current-buffer scratchpad-buffer-name
    (local-set-key (kbd scratchpad-menu-key) #'scratchpad-help)))

(defun scratchpad-other-window ()
  "Open the *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scratchpad-buffer-name)))

(defun scratchpad-restore-contents ()
  "Restore the latest heading from the scratchpad file contents."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (erase-buffer)
    (when (file-exists-p scratchpad-save-file)
      (with-temp-buffer
        (insert-file-contents scratchpad-save-file)
        (goto-char (point-max))
        (when (re-search-backward "^\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t)  ; Find last timestamp heading
          (let ((start (point)))
            (forward-line)  ; Skip the heading line
            (let ((content (buffer-substring-no-properties (point) (point-max))))
              (with-current-buffer scratchpad-buffer-name
                (insert content)))))))))

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
          (set-mark (point))
          (insert selected-text "\n"))))
    (scratchpad-other-window)))

(defun scratchpad-save-current ()
  "Save the contents of '*scratch*' buffer to `scratchpad-save-file` as a heading."
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d %I:%M %p"))
         (content (with-current-buffer scratchpad-buffer-name
                    (buffer-string))))
    (when (and scratchpad-buffer-name (not (string-empty-p content)))
      (with-temp-buffer
        (insert (concat "* " timestamp "\n" content "\n"))
        (append-to-file (point-min) (point-max) scratchpad-save-file))
      (with-current-buffer scratchpad-buffer-name
        (erase-buffer)))))

(defun scratchpad-toggle-new ()
  "Start a new scratchpad. This will save and then wipe the `*scratch*' buffer."
  (interactive)
  (with-current-buffer scratchpad-buffer-name
    (scratchpad-save-current)
    (erase-buffer)))

(transient-define-prefix scratchpad-menu ()
  "Scratchpad menu."
  ["Actions"
   ("n" "New scratchpad" scratchpad-toggle-new)
   ("." "Quit" transient-quit-one)])

(defun scratchpad-help ()
  "Show the scratchpad help menu."
  (interactive)
  (scratchpad-menu))

(define-key (current-global-map) (kbd scratchpad-menu-key) #'scratchpad-help)

(provide 'scratchpad)
(scratchpad-initialize)
;;; scratchpad.el ends here
