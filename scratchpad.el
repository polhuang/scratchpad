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

(defcustom scratchpad-save-directory
  (expand-file-name "scratchpad" "~/org")
  "Directory where scratchpad files are stored."
  :type 'directory
  :group 'scratchpad)

(defcustom scratchpad-current-backup-file
  (expand-file-name "current-scratch.org" scratchpad-save-directory)
  "Current file storing scratchpad contents."
  :type 'file
  :group 'scratchpad)
  
(defcustom scratchpad-archive-filename-format "%Y-%m-%d--%H-%M-%S-%N"
  "Format of backup file names, for `format-time-string'."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-autosave-interval 60
  "The interval (in seconds) at which scratchpad will backup to file."
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
  (unless (file-exists-p scratchpad-save-directory)
    (make-directory scratchpad-save-directory t))
  (scratchpad-restore-contents)
  (with-current-buffer scratchpad-buffer-name
    (unless (eq major-mode scratchpad-major-mode-initial)
      (funcall scratchpad-major-mode-initial))
    (scratchpad-mode 1)
    (run-hooks 'scratchpad-initialize-hook)))

;;;###autoload
(defun scratchpad-toggle ()
  "Toggle scratchpad buffer."
  (interactive)
  (if (string= (buffer-name) scratchpad-buffer-name)
      (progn
        (scratchpad-save-buffer)  ; Save only when we're in the scratchpad buffer
        (delete-window))
    (let ((selected-text (when (region-active-p)
                          (buffer-substring-no-properties (region-beginning) (region-end)))))
      (when selected-text
        (with-current-buffer (get-buffer-create scratchpad-buffer-name)
          (goto-char (point-max))
          (set-mark (point))
          (insert selected-text "\n"))))
    (scratchpad-other-window)))

;;;###autoload
(defun scratchpad-other-window ()
  "Open the *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scratchpad-buffer-name)))

(defun scratchpad-restore-contents ()
  "Restore the latest heading from the scratchpad file contents."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (erase-buffer)
    (when (file-exists-p scratchpad-current-backup-file)
      (with-temp-buffer
        (insert-file-contents scratchpad-current-backup-file)
        (goto-char (point-max))
        (when (re-search-backward "^\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" nil t)
          (let ((start (point)))
            (forward-line)
            (let ((content (buffer-substring-no-properties (point) (point-max))))
              (with-current-buffer scratchpad-buffer-name
                (insert content)))))))))

(defun scratchpad-save-buffer ()
  "Save the contents of '*scratch*' buffer to `scratchpad-current-backup-file` so that it can be restored later."
  (interactive)
  (with-current-buffer scratchpad-buffer-name
    (write-region (point-min) (point-max) scratchpad-current-backup-file)))    

(defun scratchpad-archive-buffer ()
  "Save the contents of '*scratch*' buffer to a new file with a timestamp heading."
  (interactive)
  (let* ((timestamp (format-time-string "%Y-%m-%d %I:%M %p"))
         (archive-file (expand-file-name 
                       (format-time-string scratchpad-archive-filename-format)
                       scratchpad-save-directory))
         (content (with-current-buffer scratchpad-buffer-name
                   (buffer-string))))
    (when (and scratchpad-buffer-name (not (string-empty-p content)))
      (unless (file-exists-p archive-file)
        (with-temp-file archive-file
          (insert (concat "* " timestamp "\n"))))
      (with-temp-buffer
        (insert (concat "* " timestamp "\n" content "\n"))
        (append-to-file (point-min) (point-max) archive-file)))))

(defun scratchpad-toggle-new ()
  "Start a new scratchpad. This will save and then wipe the `*scratch*' buffer."
  (interactive)
  (with-current-buffer scratchpad-buffer-name
    (scratchpad-archive-buffer)
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

(defvar scratchpad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd scratchpad-menu-key) #'scratchpad-help)
    map)
  "Keymap for `scratchpad-mode'.")

(define-minor-mode scratchpad-mode
  "Minor mode for scratchpad functionality.
When enabled, provides keybindings and functionality for scratchpad operations."
  :lighter " Scratch"
  :keymap scratchpad-mode-map
  :group 'scratchpad)

(provide 'scratchpad)
(scratchpad-initialize)

(run-with-timer 0 scratchpad-autosave-interval #'scratchpad-save-buffer)
;;; scratchpad.el ends here
