# scratchpad.el

An improved scratch buffer for Emacs with auto-save and per-file scratchpads.

Unlike the default `*scratch*` buffer, which disappears or resets on restart, `scratchpad.el` automatically persists your scratch notes to disk. It also supports file-specific scratch buffers, giving you a dedicated place to jot down notes associated with each file.

---

## Features

- **Global scratchpad**  
  - `*scratch*` buffer is auto-saved to `scratchpad-current-file`.  
  - Archives old sessions to timestamped files.  
  - Configurable major mode (defaults to `org-mode`).  

- **Per-file scratchpads**  
  - Each visited file can have its own scratchpad.  
  - Buffers are named like:  
    ```
    *scratch* [example.org - Documents]
    ```
  - Content is saved to a unique file under:  
    ```
    <scratchpad-save-directory>/file-associated/
    ```
  - Never visits the backing file directly - safe to experiment in.  

- **Autosave everywhere**  
  - Saves all scratchpads on a timer (`scratchpad-autosave-interval`).  
  - Saves on Emacs exit (optional).  
  - Saves on focus change (optional).  

- **Convenient commands**  
  - `scratchpad-toggle` - toggle the global scratchpad.  
  - `scratchpad-open-for-current-file` - open a per-file scratchpad in another window.  
  - `scratchpad-toggle-for-current-file` - toggle a per-file scratchpad for the current buffer.  
  - `scratchpad-save-buffer` - save current (or given) scratchpad to its backing file.  
  - `scratchpad-save-all-buffers` - save all scratchpads at once.  
  - `scratchpad-toggle-new` - archive and start a fresh global scratchpad.  

- **Transient menu** (incomplete):  
  - Open per-file scratchpad  
  - Toggle per-file scratchpad  
  - New global scratchpad  
  - Save scratchpad  

---

## Installation

Clone or download this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "~/path/to/scratchpad.el")
(require 'scratchpad)
