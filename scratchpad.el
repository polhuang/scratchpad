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
;;; Buffer-local state

(defvar-local scratchpad-associated-file nil
  "If non-nil, path to the backing file for this scratchpad buffer.")

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

(defun scratchpad--find-buffer-by-associated-file (file)
  "Return an open scratchpad buffer whose `scratchpad-associated-file' is FILE, or nil."
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
  "Return the per-file scratchpad path for FILE (or current buffer's file)."
  (let* ((src (or file
                  (buffer-file-name)
                  (user-error "Current buffer is not visiting a file")))
         (safe (scratchpad--sanitize-path-for-filename (expand-file-name src)))
         (dir  (scratchpad--ensure-dir scratchpad-file-assoc-directory)))
    (expand-file-name (format scratchpad-file-assoc-filename-format safe) dir)))

;;;###autoload
(defun scratchpad-open-for-current-file ()
  "Open (or create) a scratchpad buffer associated with the current file.

Always opens in another window.

Creates a buffer named \"*scratch* [<basename>]\", inserts the contents
of the associated scratchpad file (if any), and enables `scratchpad-mode`.
The buffer does not visit any file; saving is handled by scratchpad APIs."
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
  "Open (or create) a scratchpad buffer associated with FILE.
With OTHER-WINDOW non-nil, open in another window.

Creates a buffer named \"*scratch* [<basename>]\", inserts the current contents
of the associated backing file (if it exists), and enables `scratchpad-mode`.
The buffer does not visit any file; saving is handled by scratchpad APIs."
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

;;;###autoload
(defun scratchpad-restore-contents ()
  "Restore all contents from the scratchpad backup file."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (erase-buffer)
    (when (file-exists-p scratchpad-current-file)
      (insert-file-contents scratchpad-current-file))))

;;;###autoload
(defun scratchpad-save-buffer (&optional buffer)
  "Save a scratchpad BUFFER to its backing file.

If BUFFER is non-nil, save that buffer.  
If BUFFER is nil, save the current buffer.

- For file-associated scratchpads, saves to their buffer-local
  `scratchpad-associated-file`.
- For the main `*scratch*` buffer, saves to `scratchpad-current-file`."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'scratchpad-mode)
        (user-error "Not in a scratchpad buffer: %s" (buffer-name buf)))
      (let ((dest (or scratchpad-associated-file
                      (and (string= (buffer-name buf) scratchpad-buffer-name)
                           scratchpad-current-file))))
        (unless dest
          (user-error "No associated backing file for this scratchpad: %s"
                      (buffer-name buf)))
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
  "Save all open scratchpad buffers (global and file-associated).
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
      (message "Saved %d scratchpad buffer%s"
               count (if (= count 1) "" "s")))
    count))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun scratchpad-menu-open ()
  "Show the scratchpad help menu."
  (interactive)
  (scratchpad-menu))

(defun scratchpad-save-before-exit ()
  "Conditionally save the scratchpad buffer before exiting Emacs."
  (when (and scratchpad-autosave-on-exit
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-all-buffers)))

(defun scratchpad--autosave-on-focus-change ()
  "Auto-save scratchpad buffer when Emacs frame loses focus."
  (when (and scratchpad-autosave-on-focus-change
             (get-buffer scratchpad-buffer-name))
    (scratchpad-save-all-buffers)))

(add-hook 'kill-emacs-hook #'scratchpad-save-before-exit)
(add-hook 'focus-out-hook #'scratchpad--autosave-on-focus-change)
(run-with-timer 0 scratchpad-autosave-interval #'scratchpad-save-buffer)
(scratchpad-initialize)

(provide 'scratchpad)
;;; scratchpad.el ends here
