;;; scratchpad.el --- An enhanced scratch buffer with autosave and file-specific notes  -*- lexical-binding: t; -*-
;; Copyright (c) 2023-2025 Paul Huang
;;
;; Author: Paul Huang <paulleehuang@protonmail.com>
;; URL: https://github.com/polhuang/scratchpad
;;
;; This file is not part of GNU Emacs.
;;
;; Created: December 31, 2023
;; Keywords: scratch, capture, notes, note-taking
;; Package-Requires: ((emacs "26.1") (transient "0.3.7"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Enhanced scratch buffer within Emacs. This package provides a persistent
;; scratch buffer with autosave, archiving, and text capturing capabilities.
;; It also supports creating scratch buffers linked to individual files,
;; giving you a dedicated place to jot down notes associated with each file.
;;
;; Quick start:
;;   (require 'scratchpad)
;;   (scratchpad-enable)
;;
;; Useful commands:
;;   M-x scratchpad-toggle         ; pop open/close the main *scratch* window
;;   M-x scratchpad-toggle-new     ; archive current, start a fresh scratch
;;   M-x scratchpad-open-for-current-file ; per-file scratch buffer
;;   M-x scratchpad-save-all-buffers      ; save all scratch buffers to disk
;;
;; Customize via `M-x customize-group RET scratchpad RET`.

;;; Code:

(require 'transient)
(require 'seq)
(require 'subr-x)

;;
;;; Customization

(defgroup scratchpad nil
  "Scratch buffer with capture and edit."
  :group 'files
  :prefix "scratchpad-")

(defcustom scratchpad-buffer-name "*scratch*"
  "Name of the main scratchpad buffer."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-save-directory
  (expand-file-name "scratchpad" "~/org")
  "Directory where scratchpad files are stored."
  :type 'directory
  :group 'scratchpad)

(defcustom scratchpad-file-assoc-directory
  (expand-file-name "file-associated" scratchpad-save-directory)
  "Directory for file-associated scratchpad files.

By default this is the \"file-associated\" subfolder of
`scratchpad-save-directory`."
  :type 'directory
  :group 'scratchpad)

(defcustom scratchpad-current-file
  (expand-file-name "current-scratch.org" scratchpad-save-directory)
  "Backing file for the main scratchpad."
  :type 'file
  :group 'scratchpad)

(defcustom scratchpad-current-metadata-file
  (expand-file-name ".scratchpad-current" scratchpad-save-directory)
  "File preserving metadata (creation time) for the main scratchpad."
  :type 'file
  :group 'scratchpad)

(defcustom scratchpad-archive-filename-format "%Y-%m-%d.org"
  "Format of archive filenames, for `format-time-string'."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-file-assoc-filename-format "%s.org"
  "Format string for file-associated scratch filenames.

It receives one argument: a sanitized absolute path of the source file.
The result is joined under `scratchpad-file-assoc-directory`.

Example:
  visiting /home/pol/Documents/example.org
  → scratchpad/file-associated/__home__pol__Documents__example.org.org"
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-autosave-interval 60
  "Interval (seconds) between periodic autosaves of scratch buffers."
  :type 'number
  :group 'scratchpad)

(defcustom scratchpad-major-mode-initial
  (if (fboundp 'org-mode) 'org-mode 'lisp-interaction-mode)
  "Initial major mode for the main scratchpad buffer.

Defaults to `org-mode` if available; otherwise `lisp-interaction-mode`."
  :type 'function
  :group 'scratchpad)

(defcustom scratchpad-menu-key "C-c s"
  "Key binding to open the scratchpad menu in scratch buffers."
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-autosave-on-exit t
  "If non-nil, automatically save scratch buffers on Emacs exit."
  :type 'boolean
  :group 'scratchpad)

(defcustom scratchpad-autosave-on-focus-change nil
  "If non-nil, save scratch buffers when Emacs loses focus."
  :type 'boolean
  :group 'scratchpad)

(defvar scratchpad--autosave-timer nil
  "Timer for periodic scratchpad autosaves.")

;;
;;; Mode & keymap

(defvar scratchpad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd scratchpad-menu-key) #'scratchpad-menu-open)
    map)
  "Keymap for `scratchpad-mode'.")

;;;###autoload
(define-derived-mode scratchpad-mode text-mode "Scratch"
  "Special mode for scratchpad buffers.")

;;
;;; Buffer-local state

(defvar-local scratchpad-associated-file nil
  "If non-nil, path to backing file for this scratchpad buffer.")

;;
;;; Utilities

(defun scratchpad--ensure-dir (dir)
  "Ensure DIR exists and return it."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun scratchpad--sanitize-path-for-filename (path)
  "Turn PATH into a filesystem-safe single filename."
  (let* ((abs (expand-file-name path))
         (safe (replace-regexp-in-string "[/\\\\:*?\"<>|]" "__" abs)))
    (replace-regexp-in-string "__+" "__" safe)))

(defun scratchpad--find-buffer-by-associated-file (file)
  "Return an open scratchpad buffer whose associated file matches FILE.

Checks the buffer-local `scratchpad-associated-file`.  Return nil if none."
  (let ((abs (expand-file-name file)))
    (seq-find
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'scratchpad-mode)
              (string= (expand-file-name (or scratchpad-associated-file "")) abs))))
     (buffer-list))))

(defun scratchpad--make-buffer-name (src)
  "Return a buffer name for SRC: *scratch* [basename - parent]."
  (let* ((base   (file-name-nondirectory src))
         (parent (file-name-nondirectory
                  (directory-file-name (file-name-directory src)))))
    (generate-new-buffer-name
     (format "*scratch* [%s - %s]" base parent))))

;;
;;; File-associated scratchpads

;;;###autoload
(defun scratchpad-file-associated-path (&optional file)
  "Return the per-file scratchpad path for FILE or the current buffer's file."
  (let* ((src (or file
                  (buffer-file-name)
                  (user-error "Current buffer is not visiting a file")))
         (safe (scratchpad--sanitize-path-for-filename (expand-file-name src)))
         (dir  (scratchpad--ensure-dir scratchpad-file-assoc-directory)))
    (expand-file-name (format scratchpad-file-assoc-filename-format safe) dir)))

;;;###autoload
(defun scratchpad-open-for-current-file ()
  "Open/create a scratchpad buffer associated with the current file.

Opens in another window, loads contents if they exist, and enables
`scratchpad-mode`. The buffer does not visit a file; saving is
handled by scratchpad APIs."
  (interactive)
  (let* ((src (or (buffer-file-name)
                  (user-error "Current buffer is not visiting a file")))
         (dest (scratchpad-file-associated-path src))
         (existing (scratchpad--find-buffer-by-associated-file dest))
         (buf (or existing (get-buffer-create (scratchpad--make-buffer-name src))))
         (newp (and (not (file-exists-p dest)) (null existing))))
    (with-current-buffer buf
      (unless existing
        (erase-buffer)
        (when (file-exists-p dest) (insert-file-contents dest))
        (when newp (insert (format "#+TITLE: Scratch for %s\n\n" src)))
        (unless (eq major-mode scratchpad-major-mode-initial)
          (funcall scratchpad-major-mode-initial))
        (scratchpad-mode)
        (setq-local scratchpad-associated-file dest)))
    (switch-to-buffer-other-window buf)
    (message "Scratchpad for file: %s" dest)))

;;;###autoload
(defun scratchpad-open-for-file (file &optional other-window)
  "Open/create a scratchpad buffer associated with FILE.

With OTHER-WINDOW non-nil, open in another window."
  (interactive "fFile: \nP")
  (let* ((dest (scratchpad-file-associated-path file))
         (bufname (scratchpad--make-buffer-name file))
         (newp (not (file-exists-p dest)))
         (buf (get-buffer-create bufname)))
    (scratchpad--ensure-dir (file-name-directory dest))
    (with-current-buffer buf
      (erase-buffer)
      (when (file-exists-p dest)
        (insert-file-contents dest))
      (when (and newp (fboundp 'org-mode))
        (insert (format "#+TITLE: Scratch for %s\n\n" file)))
      (unless (eq major-mode scratchpad-major-mode-initial)
        (funcall scratchpad-major-mode-initial))
      (scratchpad-mode)
      (setq-local scratchpad-associated-file dest))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Scratchpad for file: %s" dest)))

;;
;;; Core scratchpad functions

;;;###autoload
(defun scratchpad-initialize ()
  "Initialize the main scratchpad buffer and directories."
  (setq initial-scratch-message nil)
  (scratchpad--ensure-dir scratchpad-save-directory)
  (scratchpad--ensure-dir scratchpad-file-assoc-directory)
  (unless (file-exists-p scratchpad-current-metadata-file)
    (with-temp-file scratchpad-current-metadata-file
      (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
    (message "Scratchpad metadata file created"))
  (scratchpad-restore-contents)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (unless (eq major-mode scratchpad-major-mode-initial)
      (funcall scratchpad-major-mode-initial))
    (scratchpad-mode)
    (run-hooks 'scratchpad-initialize-hook)))

;;;###autoload
(defun scratchpad-toggle ()
  "Toggle the main scratchpad window.

If currently in the scratchpad buffer, save and close its window.
If text is selected in the current buffer, append it to scratch."
  (interactive)
  (if (string= (buffer-name) scratchpad-buffer-name)
      (progn
        (scratchpad-save-buffer)  ;; only when inside scratch
        (delete-window))
    (let ((selected-text (when (region-active-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end)))))
      (when selected-text
        (with-current-buffer (get-buffer-create scratchpad-buffer-name)
          (goto-char (point-max))
          (set-mark (point))
          (insert selected-text "\n"))))
    (scratchpad-other-window)))

;;;###autoload
(defun scratchpad-other-window ()
  "Open the main *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scratchpad-buffer-name)))

;;;###autoload
(defun scratchpad-restore-contents ()
  "Restore contents of the main scratchpad from its backing file."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (erase-buffer)
    (when (file-exists-p scratchpad-current-file)
      (insert-file-contents scratchpad-current-file))))

;;;###autoload
(defun scratchpad-save-buffer (&optional buffer)
  "Save a scratchpad BUFFER to its backing file.

If BUFFER is nil, save the current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'scratchpad-mode)
        (user-error "Not in a scratchpad buffer: %s" (buffer-name buf)))
      (let ((dest (or scratchpad-associated-file
                      (and (string= (buffer-name buf) scratchpad-buffer-name)
                           scratchpad-current-file))))
        (unless dest
          (user-error "No associated backing file for: %s" (buffer-name buf)))
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) dest))
        (when (string= (buffer-name buf) scratchpad-buffer-name)
          ;; keep metadata for main scratch
          (unless (file-exists-p scratchpad-current-metadata-file)
            (with-temp-file scratchpad-current-metadata-file
              (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))))
        (message "Scratchpad saved: %s" dest)))))

;;;###autoload
(defun scratchpad-save-all-buffers ()
  "Save all open scratchpad buffers (main and file-associated).

Returns the number of buffers saved."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'scratchpad-mode)
            (condition-case err
                (progn
                  (scratchpad-save-buffer buf)
                  (setq count (1+ count)))
              (error
               (message "scratchpad: failed to save %s: %s"
                        (buffer-name buf) err)))))))
    (when (called-interactively-p 'interactive)
      (message "Saved %d scratchpad buffer%s" count (if (= count 1) "" "s")))
    count))

;;;###autoload
(defun scratchpad-archive-buffer ()
  "Archive the main scratchpad contents to a dated Org file.

Records both creation and archive timestamps in a PROPERTIES drawer."
  (interactive)
  (let* ((scratch-content (with-current-buffer (get-buffer-create scratchpad-buffer-name)
                            (buffer-string)))
         (created-ts (or (ignore-errors
                           (date-to-time
                            (string-trim
                             (with-temp-buffer
                               (insert-file-contents scratchpad-current-metadata-file)
                               (buffer-string)))))
                         (current-time)))
         (archive-file (expand-file-name
                        (format-time-string scratchpad-archive-filename-format created-ts)
                        scratchpad-save-directory))
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

;;;###autoload
(defun scratchpad-toggle-new ()
  "Archive and wipe the main scratchpad, then start a new one."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (scratchpad-archive-buffer)
    (erase-buffer))
  (scratchpad--ensure-dir scratchpad-save-directory)
  (with-temp-file scratchpad-current-metadata-file
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
  (message "Scratchpad reset; new scratch session starts now."))

(transient-define-prefix scratchpad-menu ()
  "Scratchpad menu."
  ["Actions"
   ("n" "New scratchpad" scratchpad-toggle-new)
   ("." "Quit" transient-quit-one)])

;;;###autoload
(defun scratchpad-menu-open ()
  "Show the scratchpad menu."
  (interactive)
  (scratchpad-menu))

;;;; Transient menu helpers

(defun scratchpad-toggle-autosave-on-exit ()
  "Toggle whether scratch buffers are saved on Emacs exit."
  (interactive)
  (setq scratchpad-autosave-on-exit (not scratchpad-autosave-on-exit))
  (message "scratchpad: save on exit %s"
           (if scratchpad-autosave-on-exit "ENABLED" "DISABLED")))

(defun scratchpad-toggle-autosave-on-focus-change ()
  "Toggle whether scratch buffers are saved when Emacs loses focus."
  (interactive)
  (setq scratchpad-autosave-on-focus-change (not scratchpad-autosave-on-focus-change))
  (message "scratchpad: save on focus change %s"
           (if scratchpad-autosave-on-focus-change "ENABLED" "DISABLED")))

(defun scratchpad-set-autosave-interval (seconds)
  "Set autosave interval for scratch buffers to SECONDS and restart the timer."
  (interactive "nAutosave interval (seconds): ")
  (setq scratchpad-autosave-interval seconds)
  (when (timerp scratchpad--autosave-timer)
    (cancel-timer scratchpad--autosave-timer)
    (setq scratchpad--autosave-timer
          (run-with-timer 0 scratchpad-autosave-interval
                          #'scratchpad-save-all-buffers)))
  (message "scratchpad: autosave interval set to %s seconds" seconds))

;;;; Transient menu

(transient-define-prefix scratchpad-menu ()
  "Scratchpad menu."
  [
   ;; Column 1: Global scratch
   ["Global scratch buffer"
    ("n" "New scratch buffer"                   scratchpad-toggle-new)
    ]

   ["Save scratch buffer"
    ("s" "Save current scratch buffer"          scratchpad-save-buffer)
    ("S" "Save ALL scratch buffers"             scratchpad-save-all-buffers)]
   
   ;; Column 2: File-linked scratchpads
   ["Open scratch buffer"
    ("f" "Open scratchpad for current file"     scratchpad-open-for-current-file)
    ("F" "Open for file…"                       scratchpad-open-for-file)]

   ;; Column 3: Help
   ["Essential commands"
    ("." "Quit"                                 transient-quit-one)]
   ])

;;;###autoload
(defun scratchpad-menu-open ()
  "Show the scratchpad menu."
  (interactive)
  (scratchpad-menu))

;;
;;; Lifecycle (opt-in)

(defun scratchpad-save-before-exit ()
  "Save scratch buffers before exiting Emacs, if enabled."
  (when (and scratchpad-autosave-on-exit
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-all-buffers)))

(defun scratchpad--autosave-on-focus-change ()
  "Auto-save scratch buffers when Emacs frame loses focus."
  (when (and scratchpad-autosave-on-focus-change
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-all-buffers)))

;;;###autoload
(defun scratchpad-enable ()
  "Enable Scratchpad autosave and hooks; also initialize the main buffer."
  (interactive)
  (add-hook 'kill-emacs-hook #'scratchpad-save-before-exit)
  (add-hook 'focus-out-hook #'scratchpad--autosave-on-focus-change)
  (unless (timerp scratchpad--autosave-timer)
    (setq scratchpad--autosave-timer
          (run-with-timer 0 scratchpad-autosave-interval
                          #'scratchpad-save-all-buffers)))
  (scratchpad-initialize))

;;;###autoload
(defun scratchpad-disable ()
  "Disable Scratchpad autosave and hooks; cancel timers."
  (interactive)
  (remove-hook 'kill-emacs-hook #'scratchpad-save-before-exit)
  (remove-hook 'focus-out-hook #'scratchpad--autosave-on-focus-change)
  (when (timerp scratchpad--autosave-timer)
    (cancel-timer scratchpad--autosave-timer)
    (setq scratchpad--autosave-timer nil)))

(provide 'scratchpad)
;;; scratchpad.el ends here
