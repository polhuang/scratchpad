;;; scratchpad.el --- Use the scratch buffer as a capture and edit space -*- lexical-binding: t -*-

;; Author: Paul Huang <polhuang@proton.me>
;; URL: https://github.com/polhuang/scratchpad
;; Package-Version: 0.0.0
;; Package-Requires: ((emacs "28"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2023-2024 Paul Huang
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; TBD

;;; Code:
(setq initial-major-mode 'org-mode
      initial-scratch-message nil)

(defgroup scratchpad nil
  "filler"
  :group 'files
  :prefix "scratchpad-")
  
(defcustom scratchpad-buffer-name "*scratch*"
  "Custom scratchpad buffer name."
  :type 'string
  :group 'persistent-scratch)

(defcustom scratchpad-save-file
  (expand-file-name "scratchpad.org" user-emacs-directory)
  "File storing scratchpad contents."
  :type 'file
  :group 'persistent-scratch)

(defun scratchpad-restore-contents ()
  "Restore scratchpad file contents."
  (interactive)
  (with-current-buffer (get-buffer-create scratchpad-buffer-name)
    (insert-file-contents scratchpad-save-file)))

(defun scratchpad-other-window ()
  "Open the *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create scratchpad-buffer-name)))

(defun scratchpad-toggle-scratchpad ()
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


(provide 'scratchpad)
;;; scratchpad.el ends here
