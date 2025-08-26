;;; scratchpad.el --- An enhanced scratch buffer with autosave and file-specific notes  -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Paul Huang

;; Author: Paul Huang <paulleehuang@protonmail.com>
;; Created: December 31, 2023
;; Package-Requires: ((emacs "26.1") (transient "0.3.7") (org "9.0"))
;; Keywords: scratch, capture, notes, note-taking
;; URL: https://github.com/polhuang/scratchpad

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; scratchpad provides a persistent scratch buffer system with autosave, archiving,
;; and text capturing capabilities. It supports three types of scratchpad buffers:
;; main, file-associated, and named scratchpads.
;;
;; Features:
;; - Main scratchpad: Persistent buffer with periodic autosave and archiving
;; - File-associated scratchpads: Dedicated scratch buffers linked to specific files
;; - Named scratchpads: Custom-named scratch buffers for organizing notes by topic
;; - Smart archiving: Main scratchpad saves by overwriting, others append with timestamps
;; - Text capture: Automatically append selected text when opening scratchpad buffers
;; - Latest entry viewing: Quick access to most recent content from archived entries
;; - Cycling navigation: Easy switching between different scratchpad types
;; - Integrated with org-mode for structured note-taking
;; - Transient menu interface for all operations
;;
;; Key Functions:
;; - `scratchpad-toggle`: Smart toggle between scratchpad buffers
;; - `scratchpad-cycle`: Cycle through main, file-associated, and named scratchpads
;; - `scratchpad-open-main`: Open main scratchpad (captures selected text)
;; - `scratchpad-open-for-current-file`: Create/open scratchpad for current file
;; - `scratchpad-open-named`: Create/open named scratchpad
;; - `scratchpad-open-latest-*`: View latest archived entries
;; - `scratchpad-new`: Archive current content and start fresh
;; - `scratchpad-save-buffer`: Save/archive scratchpad content
;;


;;; Code:

(require 'transient)
(require 'seq)
(require 'subr-x)
(require 'org)

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

(defcustom scratchpad-named-directory
  (expand-file-name "named" scratchpad-save-directory)
  "Directory for named scratchpad files.

By default this is the \"named\" subfolder of
`scratchpad-save-directory`."
  :type 'directory
  :group 'scratchpad)

(defcustom scratchpad-current-file
  (expand-file-name "current-scratch.org" scratchpad-save-directory)
  "Backing file for the main scratch."
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

(defcustom scratchpad-named-filename-format "%s.org"
  "Format string for named scratch filenames.

It receives one argument: the name of the scratch buffer.
The result is joined under `scratchpad-named-directory`.

Example:
  named buffer \"ideas\"
  → scratchpad/named/ideas.org"
  :type 'string
  :group 'scratchpad)

(defcustom scratchpad-autosave-interval 60
  "Interval (seconds) between periodic autosaves of scratch buffers."
  :type 'number
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
    (define-key map (kbd "C-c C-.") #'scratchpad-menu-open)
    (define-key map (kbd "C-c C-s") #'scratchpad-save-buffer)
    (define-key map (kbd "C-c C-a") #'scratchpad-archive-buffer)
    (define-key map (kbd "C-c M-a") #'scratchpad-archive-named-buffer)
    (define-key map (kbd "C-c C-n") #'scratchpad-new)
    map)
  "Keymap for `scratchpad-mode'.")

;;;###autoload
(define-derived-mode scratchpad-mode org-mode "Scratch"
  "Special mode for scratchpad buffers derived from Org.")

;;
;;; Buffer-local state

(defvar-local scratchpad-associated-file nil
  "If non-nil, path to backing file for this scratchpad buffer.")

(defvar-local scratchpad-buffer-name-local nil
  "If non-nil, the name of this named scratchpad buffer.")

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

(defun scratchpad--find-scratchpad-by-associated-file (file)
  "Return an open scratchpad whose associated file matches FILE.

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
;;; Named scratchpads

(defun scratchpad--find-named-scratchpad (name)
  "Return an open named scratchpad with NAME.

Checks the buffer-local `scratchpad-buffer-name-local`.  Return nil if none."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'scratchpad-mode)
            (string= scratchpad-buffer-name-local name))))
   (buffer-list)))

(defun scratchpad-named-path (name)
  "Return the path for named scratchpad buffer NAME."
  (let ((dir (scratchpad--ensure-dir scratchpad-named-directory)))
    (expand-file-name (format scratchpad-named-filename-format name) dir)))

;;;###autoload
(defun scratchpad-open-named (name &optional other-window)
  "Open or create a named scratchpad buffer with NAME.

With OTHER-WINDOW non-nil, open in another window."
  (interactive
   (list (completing-read
          "Open named scratchpad: "
          (let ((dir scratchpad-named-directory))
            (when (file-directory-p dir)
              (mapcar (lambda (f) (file-name-sans-extension f))
                      (directory-files dir nil "\\.org$"))))
          nil nil)
         current-prefix-arg))
  (when (string-empty-p name)
    (user-error "Name cannot be empty"))
  (let* ((dest (scratchpad-named-path name))
         (existing (scratchpad--find-named-scratchpad name))
         (bufname (format "*scratch* [%s]" name))
         (buf (or existing (get-buffer-create bufname)))
         (newp (and (not (file-exists-p dest)) (null existing))))
    (with-current-buffer buf
      (unless existing
        (erase-buffer)
        (when newp
          (with-temp-file dest
            (insert (format "#+TITLE: %s\n\n" name))))
        (when (file-exists-p dest)
          (insert-file-contents dest)
          (goto-char (point-min))
          (when (looking-at "^#\\+TITLE:.*\n\\(?:\n\\)?")
            (delete-region (match-beginning 0) (match-end 0))))
        (scratchpad-mode)
        (setq-local scratchpad-associated-file dest)
        (setq-local scratchpad-buffer-name-local name)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Named scratchpad: %s" name)))

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
         (existing (scratchpad--find-scratchpad-by-associated-file dest))
         (buf (or existing (get-buffer-create (scratchpad--make-buffer-name src))))
         (newp (and (not (file-exists-p dest)) (null existing))))
    (with-current-buffer buf
      (unless existing
        (erase-buffer)
        (when (file-exists-p dest) (insert-file-contents dest))
        (when newp (insert (format "#+TITLE: Scratch for %s\n\n" src)))
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
      (when newp
        (insert (format "#+TITLE: Scratch for %s\n\n" file)))
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
  (scratchpad--ensure-dir scratchpad-named-directory)
  (unless (file-exists-p scratchpad-current-metadata-file)
    (with-temp-file scratchpad-current-metadata-file
      (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
    (message "Scratchpad metadata file created"))
  (scratchpad-restore-contents)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (scratchpad-mode)
    (run-hooks 'scratchpad-initialize-hook)))

;;;###autoload
(defun scratchpad-toggle ()
  "Toggle scratchpad buffers.

If currently in a scratchpad buffer, save and close its window.
Otherwise, open the main scratchpad buffer."
  (interactive)
  (if (derived-mode-p 'scratchpad-mode)
      (progn
        (scratchpad-save-buffer)
        (delete-window))
    (scratchpad-open-main t)))

;;;###autoload
(defun scratchpad-open-main (&optional other-window)
  "Open the main scratchpad buffer.

If text is selected in the current buffer, append it to the main scratchpad.
With OTHER-WINDOW non-nil, open in another window."
  (interactive "P")
  (let ((selected-text (when (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))))
        (buf (get-buffer-create scratchpad-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'scratchpad-mode)
        (scratchpad-mode))
      (when selected-text
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert selected-text "\n")))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))))

;;;###autoload
(defun scratchpad-other-window ()
  "Open the main *scratch* buffer in a new window."
  (interactive)
  (let ((buf (get-buffer-create scratchpad-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'scratchpad-mode)
        (scratchpad-mode)))
    (switch-to-buffer-other-window buf)))

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

For main scratchpad: saves by overwriting.
For file-associated and named scratchpads: archives with timestamp.
If BUFFER is nil, save the current buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'scratchpad-mode)
        (user-error "Not in a scratchpad buffer: %s" (buffer-name buf)))
      (let ((dest (or scratchpad-associated-file
                      (and (string= (buffer-name buf) scratchpad-buffer-name)
                           scratchpad-current-file)))
            (is-main (string= (buffer-name buf) scratchpad-buffer-name))
            (buffer-content (buffer-string)))
        (unless dest
          (user-error "No associated backing file for: %s" (buffer-name buf)))
        (scratchpad--ensure-dir (file-name-directory dest))
        
        (if is-main
            ;; Main scratchpad: save by overwriting
            (progn
              (save-restriction
                (widen)
                (write-region (point-min) (point-max) dest))
              (unless (file-exists-p scratchpad-current-metadata-file)
                (with-temp-file scratchpad-current-metadata-file
                  (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time)))))
              (message "Scratchpad saved: %s" dest))
          ;; File-associated or named scratchpad: archive with timestamp
          (when (and buffer-content (not (string-empty-p buffer-content)))
            (let* ((created-ts (or (ignore-errors
                                     (nth 5 (file-attributes dest)))
                                   (current-time)))
                   (archived-ts (current-time)))
              (with-temp-buffer
                (insert (format "* %s\n" (format-time-string "%I:%M %p" archived-ts)))
                (insert ":PROPERTIES:\n")
                (insert (format ":CREATED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" created-ts)))
                (insert (format ":ARCHIVED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" archived-ts)))
                (insert ":END:\n\n")
                (insert buffer-content "\n\n")
                (append-to-file (point-min) (point-max) dest))
              (message "Scratchpad archived to %s" dest)))))))))

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
        (insert (format "* %s\n" (format-time-string "%I:%M %p" created-ts)))
        (insert ":PROPERTIES:\n")
        (insert (format ":CREATED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" created-ts)))
        (insert (format ":ARCHIVED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" archived-ts)))
        (insert ":END:\n\n")
        (insert scratch-content "\n\n")
        (append-to-file (point-min) (point-max) archive-file))
      (message "Archived scratchpad to %s" archive-file))))

;;;###autoload
(defun scratchpad-archive-named-buffer ()
  "Archive the current named scratchpad buffer to a dated Org file.

Records both creation and archive timestamps in a PROPERTIES drawer."
  (interactive)
  (unless (and (derived-mode-p 'scratchpad-mode) scratchpad-buffer-name-local)
    (user-error "Not in a named scratchpad buffer"))
  (let* ((buffer-content (buffer-string))
         (name scratchpad-buffer-name-local)
         (file-path scratchpad-associated-file)
         (created-ts (or (ignore-errors
                           (nth 5 (file-attributes file-path)))
                         (current-time)))
         (archive-file file-path)
         (archived-ts (current-time)))
    (when (and buffer-content (not (string-empty-p buffer-content)))
      (with-temp-buffer
        (insert (format "* %s\n" (format-time-string "%I:%M %p" created-ts)))
        (insert ":PROPERTIES:\n")
        (insert (format ":CREATED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" created-ts)))
        (insert (format ":ARCHIVED_AT: %s\n" (format-time-string "<%Y-%m-%d %I:%M %p>" archived-ts)))
        (insert ":END:\n\n")
        (insert buffer-content "\n\n")
        (append-to-file (point-min) (point-max) archive-file))
      (message "Archived named scratchpad '%s' to %s" name archive-file))))

;;;###autoload
(defun scratchpad-new ()
  "Archive and wipe the main scratchpad, then start a new one."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (scratchpad-archive-buffer)
    (erase-buffer))
  (scratchpad--ensure-dir scratchpad-save-directory)
  (with-temp-file scratchpad-current-metadata-file
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
  (message "Scratchpad reset; new scratch session starts now."))

;;;###autoload
(defun scratchpad-open-by-date (date &optional other-window)
  "Open an archived scratchpad by DATE.

DATE should be a string in the format used by
`scratchpad-archive-filename-format` (default: YYYY-MM-DD).

With OTHER-WINDOW non-nil, open in another window."
  (interactive
   (list (completing-read
          "Open archived scratchpad (date): "
          (directory-files scratchpad-save-directory nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$")
          nil t)
         current-prefix-arg))
  (let* ((archive-file (expand-file-name date scratchpad-save-directory))
         (bufname (format "*scratch-archive* [%s]" date))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents archive-file)
      (scratchpad-mode))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Opened archived scratchpad: %s" archive-file)))

;;;###autoload
(defun scratchpad-open-latest-by-date (date &optional other-window)
  "Open archived scratchpad by DATE showing only the most recent entry content.

With OTHER-WINDOW non-nil, open in another window."
  (interactive
   (list (completing-read
          "Open latest entry from archived scratchpad (date): "
          (directory-files scratchpad-save-directory nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$")
          nil t)
         current-prefix-arg))
  (let* ((archive-file (expand-file-name date scratchpad-save-directory))
         (bufname (format "*scratch-latest* [%s]" date))
         (buf (get-buffer-create bufname))
         (content ""))
    (with-temp-buffer
      (insert-file-contents archive-file)
      (goto-char (point-max))
      (when (re-search-backward "^\\* " nil t)
        ;; Skip the header line and properties drawer to get actual content
        (forward-line 1)
        (when (looking-at ":PROPERTIES:")
          (re-search-forward "^:END:" nil t)
          (forward-line 1))
        ;; Skip any blank lines
        (while (and (not (eobp)) (looking-at "^\\s-*$"))
          (forward-line 1))
        (let ((start (point))
              (end (if (re-search-forward "^\\* " nil t)
                       (progn
                         ;; Go back to before the next header and remove trailing whitespace
                         (goto-char (match-beginning 0))
                         (skip-chars-backward " \t\n")
                         (point))
                     ;; No next header, go to end but remove trailing whitespace
                     (goto-char (point-max))
                     (skip-chars-backward " \t\n")
                     (point))))
          (setq content (buffer-substring-no-properties start end)))))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (scratchpad-mode)
      (goto-char (point-min)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Showing latest entry from %s" archive-file)))

;;;###autoload
(defun scratchpad-open-latest-for-current-file ()
  "Open scratchpad for current file showing only the most recent entry content."
  (interactive)
  (let* ((src (or (buffer-file-name)
                  (user-error "Current buffer is not visiting a file")))
         (archive-file (scratchpad-file-associated-path src))
         (bufname (format "*scratch-latest* [%s]" (file-name-nondirectory src)))
         (buf (get-buffer-create bufname))
         (content ""))
    (when (file-exists-p archive-file)
      (with-temp-buffer
        (insert-file-contents archive-file)
        (goto-char (point-max))
        (when (re-search-backward "^\\* " nil t)
          ;; Skip the header line and properties drawer to get actual content
          (forward-line 1)
          (when (looking-at ":PROPERTIES:")
            (re-search-forward "^:END:" nil t)
            (forward-line 1))
          ;; Skip any blank lines
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1))
          (let ((start (point))
                (end (if (re-search-forward "^\\* " nil t)
                         (progn
                           ;; Go back to before the next header and remove trailing whitespace
                           (goto-char (match-beginning 0))
                           (skip-chars-backward " \t\n")
                           (point))
                       ;; No next header, go to end but remove trailing whitespace
                       (goto-char (point-max))
                       (skip-chars-backward " \t\n")
                       (point))))
            (setq content (buffer-substring-no-properties start end))))))
    (with-current-buffer buf
      (erase-buffer)
      (if (string-empty-p content)
          (insert "No archived entries found.")
        (insert content))
      (scratchpad-mode)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)
    (message "Showing latest entry for %s" src)))

;;;###autoload
(defun scratchpad-open-latest-for-file (file &optional other-window)
  "Open scratchpad for FILE showing only the most recent entry content.

With OTHER-WINDOW non-nil, open in another window."
  (interactive "fFile: \nP")
  (let* ((archive-file (scratchpad-file-associated-path file))
         (bufname (format "*scratch-latest* [%s]" (file-name-nondirectory file)))
         (buf (get-buffer-create bufname))
         (content ""))
    (when (file-exists-p archive-file)
      (with-temp-buffer
        (insert-file-contents archive-file)
        (goto-char (point-max))
        (when (re-search-backward "^\\* " nil t)
          ;; Skip the header line and properties drawer to get actual content
          (forward-line 1)
          (when (looking-at ":PROPERTIES:")
            (re-search-forward "^:END:" nil t)
            (forward-line 1))
          ;; Skip any blank lines
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1))
          (let ((start (point))
                (end (if (re-search-forward "^\\* " nil t)
                         (progn
                           ;; Go back to before the next header and remove trailing whitespace
                           (goto-char (match-beginning 0))
                           (skip-chars-backward " \t\n")
                           (point))
                       ;; No next header, go to end but remove trailing whitespace
                       (goto-char (point-max))
                       (skip-chars-backward " \t\n")
                       (point))))
            (setq content (buffer-substring-no-properties start end))))))
    (with-current-buffer buf
      (erase-buffer)
      (if (string-empty-p content)
          (insert "No archived entries found.")
        (insert content))
      (scratchpad-mode)
      (goto-char (point-min)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Showing latest entry for %s" file)))

;;;###autoload
(defun scratchpad-open-latest-named (name &optional other-window)
  "Open named scratchpad showing only the most recent entry content.

With OTHER-WINDOW non-nil, open in another window."
  (interactive
   (list (completing-read
          "Open latest entry from named scratchpad: "
          (let ((dir scratchpad-named-directory))
            (when (file-directory-p dir)
              (mapcar (lambda (f) (file-name-sans-extension f))
                      (directory-files dir nil "\\.org$"))))
          nil nil)
         current-prefix-arg))
  (let* ((archive-file (scratchpad-named-path name))
         (bufname (format "*scratch-latest* [%s]" name))
         (buf (get-buffer-create bufname))
         (content ""))
    (when (file-exists-p archive-file)
      (with-temp-buffer
        (insert-file-contents archive-file)
        (goto-char (point-max))
        (when (re-search-backward "^\\* " nil t)
          ;; Skip the header line and properties drawer to get actual content
          (forward-line 1)
          (when (looking-at ":PROPERTIES:")
            (re-search-forward "^:END:" nil t)
            (forward-line 1))
          ;; Skip any blank lines
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1))
          (let ((start (point))
                (end (if (re-search-forward "^\\* " nil t)
                         (progn
                           ;; Go back to before the next header and remove trailing whitespace
                           (goto-char (match-beginning 0))
                           (skip-chars-backward " \t\n")
                           (point))
                       ;; No next header, go to end but remove trailing whitespace
                       (goto-char (point-max))
                       (skip-chars-backward " \t\n")
                       (point))))
            (setq content (buffer-substring-no-properties start end))))))
    (with-current-buffer buf
      (erase-buffer)
      (if (string-empty-p content)
          (insert "No archived entries found.")
        (insert content))
      (scratchpad-mode)
      (goto-char (point-min)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (message "Showing latest entry for named scratchpad: %s" name)))

;;;###autoload
(defun scratchpad-cycle ()
  "Cycle between main scratchpad, current file's latest, and named scratchpads.

Cycling order:
1. Main scratchpad
2. Current file's latest scratchpad (if file exists and has entries)
3. Named scratchpads (cycling through all available ones)

The cycle state is tracked per Emacs session."
  (interactive)
  (let* ((current-buf (current-buffer))
         (current-name (buffer-name current-buf))
         (is-scratchpad (derived-mode-p 'scratchpad-mode))
         (is-main (string= current-name scratchpad-buffer-name))
         (is-latest (string-match-p "^\\*scratch-latest\\*" current-name))
         (is-named (and is-scratchpad scratchpad-buffer-name-local))
         (current-file (buffer-file-name))
         (has-file-scratchpad (and current-file
                                   (file-exists-p (scratchpad-file-associated-path current-file))))
         (named-scratchpads (when (file-directory-p scratchpad-named-directory)
                              (mapcar (lambda (f) (file-name-sans-extension f))
                                      (directory-files scratchpad-named-directory nil "\\.org$"))))
         (cycle-state (get 'scratchpad-cycle 'state))
         (named-index (get 'scratchpad-cycle 'named-index)))
    
    (cond
     ;; If not in a scratchpad or in main, try to go to file's latest
     ((or (not is-scratchpad) is-main)
      (if has-file-scratchpad
          (progn
            (put 'scratchpad-cycle 'state 'file-latest)
            (scratchpad-open-latest-for-current-file))
        ;; Skip to named if no file scratchpad
        (if named-scratchpads
            (progn
              (put 'scratchpad-cycle 'state 'named)
              (put 'scratchpad-cycle 'named-index 0)
              (scratchpad-open-latest-named (car named-scratchpads) t))
          ;; No named either, go back to main
          (progn
            (put 'scratchpad-cycle 'state 'main)
            (scratchpad-open-main t)))))
     
     ;; If in file's latest, go to first named scratchpad
     (is-latest
      (if named-scratchpads
          (progn
            (put 'scratchpad-cycle 'state 'named)
            (put 'scratchpad-cycle 'named-index 0)
            (scratchpad-open-latest-named (car named-scratchpads) t))
        ;; No named, go back to main
        (progn
          (put 'scratchpad-cycle 'state 'main)
          (scratchpad-open-main t))))
     
     ;; If in a named scratchpad, cycle to next named or back to main
     (is-named
      (let ((current-named-name scratchpad-buffer-name-local)
            (next-index (if named-index (1+ named-index) 0)))
        (if (and (< next-index (length named-scratchpads))
                 (not (string= current-named-name (nth next-index named-scratchpads))))
            (progn
              (put 'scratchpad-cycle 'named-index next-index)
              (scratchpad-open-latest-named (nth next-index named-scratchpads) t))
          ;; End of named scratchpads, go back to main
          (progn
            (put 'scratchpad-cycle 'state 'main)
            (put 'scratchpad-cycle 'named-index nil)
            (scratchpad-open-main t)))))
     
     ;; Default: go to main
     (t
      (put 'scratchpad-cycle 'state 'main)
      (scratchpad-open-main t)))))

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
   ["New scratch buffer"
    ("n" "Archive & start new scratch buffer"   scratchpad-new)]
   ["Save scratch buffer"
    ("s" "Save current scratch buffer"          scratchpad-save-buffer)
    ("S" "Save ALL scratch buffers"             scratchpad-save-all-buffers)]
   ["Open scratch buffer"
    ("f" "For current file"                     scratchpad-open-latest-for-current-file)
    ("F" "For file…"                            scratchpad-open-latest-for-file)
    ("n" "by name"                              scratchpad-open-latest-named)]
   ["Open scratchpad file"
    ("d" "By date"                              scratchpad-open-by-date)
    ("f" "For current file"                     scratchpad-open-for-current-file)
    ("F" "For file…"                            scratchpad-open-for-file)
    ("n" "by name"                              scratchpad-open-named)]
   ["Commands"
    ("'" "Cycle scratch buffers"                scratchpad-cycle)
    ("." "Quit"                                 transient-quit-one)]
   ])

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
