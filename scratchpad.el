;;; scratchpad.el --- Use the scratch buffer as a capture and edit space  -*- lexical-binding: t -*-

;; Copyright (c) 2023-2024 Paul Huang
;;
;; Author     : Paul Huang <paulleehuang@proton.me>
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

;; A persistent scratchpad with auto-save capabilities

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

(defcustom scratchpad-file-assoc-directory
  (expand-file-name "file-associated" scratchpad-save-directory)
  "Directory where file-associated scratchpad files are stored.
By default this is the \"file-associated\" subfolder of
`scratchpad-save-directory`."
  :type 'directory
  :group 'scratchpad)

(defcustom scratchpad-current-file
  (expand-file-name "current-scratch.org" scratchpad-save-directory)
  "Current file storing scratchpad contents."
  :type 'file
  :group 'scratchpad)

(defcustom scratchpad-current-metadata-file
  (expand-file-name ".scratchpad-current" scratchpad-save-directory)
  "File to preserve metadata for current scratchpad across Emacs sessions."
  :type 'file
  :group 'scratchpad)
  
(defcustom scratchpad-archive-filename-format "%Y-%m-%d.org"
  "Format of backup file names, for `format-time-string'."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-file-assoc-filename-format "%s.org"
  "Format string for file-associated scratchpad filenames.

It receives one argument: a sanitized absolute path of the source file.
The result is joined under `scratchpad-file-assoc-directory`.

Example:
  visiting /home/pol/Documents/example.org
  → scratchpad/file-associated/__home__pol__Documents__example.org.org"
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

(defcustom scratchpad-menu-key "C-c s"
  "Key binding to activate the scratchpad menu in the scratch buffer."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-autosave-on-exit t
  "If non-nil, automatically save the scratchpad before exiting Emacs."
  :type 'boolean
  :group 'scratchpad)

(defcustom scratchpad-autosave-on-focus-change nil
  "If non-nil, automatically save the scratchpad when Emacs loses focus."
  :type 'boolean
  :group 'scratchpad)

(defvar scratchpad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd scratchpad-menu-key) #'scratchpad-menu-open)
    map)
  "Keymap for `scratchpad-mode'.")

(define-derived-mode scratchpad-mode org-mode "scratch"
  "Special mode for scratchpad buffers.")

;;
;;; Utilities

(defun scratchpad--ensure-dir (dir)
  "Ensure DIR exists and return it."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun scratchpad--sanitize-path-for-filename (path)
  "Turn PATH into a filesystem-safe single filename.
Preserves enough of the original path for easy association."
  (let* ((abs (expand-file-name path))
         (safe (replace-regexp-in-string
                "[/\\\\:*?\"<>|]" "__" abs)))
    (replace-regexp-in-string "__+" "__" safe)))

;;
;;; Functions

;;;###autoload
(defun scratchpad-initialize ()
  "Format '*scratch*' buffer if already created."
  (setq initial-scratch-message nil)
  (unless (file-exists-p scratchpad-save-directory)
    (make-directory scratchpad-save-directory t))
  (unless (file-exists-p scratchpad-file-assoc-directory)
    (make-directory scratchpad-file-assoc-directory t))
  (unless (file-exists-p scratchpad-current-metadata-file)
    (with-temp-file scratchpad-current-metadata-file
      (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
    (message "Scratchpad metadata file created"))
  (scratchpad-restore-contents)
  (with-current-buffer scratchpad-buffer-name
    (unless (eq major-mode scratchpad-major-mode-initial)
      (funcall scratchpad-major-mode-initial))
    (scratchpad-mode)
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
  "Restore all contents from the scratchpad backup file."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (erase-buffer)
    (when (file-exists-p scratchpad-current-file)
      (insert-file-contents scratchpad-current-file))))

(defun scratchpad-save-buffer ()
  "Save the contents of '*scratch*' buffer.

Contents are stored to `scratchpad-current-file`."
  (interactive)
  (with-current-buffer scratchpad-buffer-name
    (write-region (point-min) (point-max) scratchpad-current-file)))

(defun scratchpad-archive-buffer ()
  "Archive the current scratchpad contents to a dated file.
Records both creation and archive timestamps in an Org PROPERTIES drawer."
  (interactive)
  (let* ((scratch-content (with-current-buffer scratchpad-buffer-name
                           (buffer-string)))
         (created-ts (or (ignore-errors
                             (date-to-time
                              (string-trim
                               (with-temp-buffer
                                 (insert-file-contents scratchpad-current-metadata-file)
                                 (buffer-string)))))
                           (current-time)))
         (archive-file (expand-file-name
                        (format-time-string scratchpad-archive-filename-format created-ts) scratchpad-save-directory))
         (archived-ts (current-time)))
    (when (and scratch-content (not (string-empty-p scratch-content)))
      (with-temp-buffer
        ;; Org heading
        (insert (format "* %s\n" (format-time-string "%I:%M %p" created-ts)))
        ;; Properties drawer
        (insert ":PROPERTIES:\n")
        (insert (format ":CREATED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" created-ts)))
        (insert (format ":ARCHIVED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" archived-ts)))
        (insert ":END:\n\n")
        ;; Content
        (insert scratch-content "\n\n")
        ;; Append to archive file
        (append-to-file (point-min) (point-max) archive-file))
      (message "Archived scratchpad to %s" archive-file))))

(defun scratchpad-toggle-new ()
  "Start a new scratchpad.

This will save and then wipe the `*scratch*' buffer,
and record *now* as the creation time."
  (interactive)
  ;; 1) Archive and wipe
  (with-current-buffer scratchpad-buffer-name
    (scratchpad-archive-buffer)
    (erase-buffer))
  ;; 2) Record the creation timestamp
  (unless (file-exists-p scratchpad-save-directory)
    (make-directory scratchpad-save-directory t))
  (with-temp-file scratchpad-current-metadata-file
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
  (message "Scratchpad reset; new scratch session starts now."))

(transient-define-prefix scratchpad-menu ()
  "Scratchpad menu."
  ["Actions"
   ("n" "New scratchpad" scratchpad-toggle-new)
   ("." "Quit" transient-quit-one)])

(defun scratchpad-menu-open ()
  "Show the scratchpad help menu."
  (interactive)
  (scratchpad-menu))

(defun scratchpad-save-before-exit ()
  "Conditionally save the scratchpad buffer before exiting Emacs."
  (when (and scratchpad-autosave-on-exit
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-buffer)))

(defun scratchpad--autosave-on-focus-change ()
  "Auto-save scratchpad buffer when Emacs frame loses focus."
  (when (and scratchpad-autosave-on-focus-change
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-buffer)))

(add-hook 'kill-emacs-hook #'scratchpad-save-before-exit)
(add-hook 'focus-out-hook #'scratchpad--autosave-on-focus-change)
(run-with-timer 0 scratchpad-autosave-interval #'scratchpad-save-buffer)
(scratchpad-initialize)

(provide 'scratchpad)
;;; scratchpad.el ends here
