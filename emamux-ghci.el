;;; emamux-ghci.el --- library for Emacs based driving of GHCI over tmux

;; Copyright (C) 2014  John P. Feltz

;; Author: John P. Feltz <jfeltz@gmail.com>
;; Keywords: comm, tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   See README.md

(require 'emamux)
(require 's)

(defvar emamux-ghci:includes
  nil "ghc include directories, e.g. (\"src\" \"tests\")")
(defvar emamux-ghci:exts
  nil "ghc language extensions, e.g. (\"GADTs\", \"UnicodeSyntax\")")

(defvar last-proj-exts nil "used for tracking sync state")
(defvar last-proj-includes nil "used for tracking sync state")
(make-local-variable 'last-proj-exts)
(make-local-variable 'last-proj-includes)

(defvar
  emamux-ghci:tmux-address
  "haskell:ghci"
  "window and pane address for repl"
  )

(defun include-arg (strings)
  "given a list of import file-paths, format an argument to ghci :set"
  (if (eq strings nil) nil (concat "-i" (s-join ":" strings)))
  )

(defun exts-arg (strings removal)
  "given a list of language extensions, format an argument to ghci :set"
  (if (eq strings nil) nil
    (let ((prefix (if removal "No" "")))
      (s-join " " (mapcar (lambda (ext) (concat "-X" prefix ext)) strings)))))

(defun emamux-ghci:issue-set (args addr)
  (if
    (eq args nil)
    (message "emamux-ghci: ghci session unchanged")
    (emamux-ghci:issue-cmd (concat ":set " args) addr)))

(defun emamux-ghci:issue-cmd (cmd addr)
  "enter the command and issue the necessary carriage return in ghci"
  (emamux:send-keys cmd addr)
  (emamux:reset-prompt addr)
  )

(defun emamux-ghci:clear-exts (exts addr)
   (if exts (emamux-ghci:issue-set (exts-arg exts t) addr) nil)
  )
(defun emamux-ghci:clear-includes (addr)
   (emamux-ghci:issue-set "-i" addr)
  )

(defun emamux-ghci:load-file (fp addr)
  "given a filepath, load a Haskell module file in ghci"
  (if
    (eq fp nil)
    (error "nil filepath provided to emamux-ghci:load-file")
    (emamux-ghci:issue-cmd (concat ":load " fp) addr)
    ))

(defun emamux-ghci:sync-includes (includes addr)
  (if includes (emamux-ghci:issue-set (include-arg includes) addr) nil))

(defun emamux-ghci:sync-exts (exts addr)
  (if exts (emamux-ghci:issue-set (exts-arg exts nil) addr) nil))

(defun emamux-ghci:proj-sync (&optional force)
  "if project settings have changed, remove the old ones, bring in new ones
  Note: (force) applies settings anyway, even if emamux-ghci thinks they're
  already applied."
  (interactive)

  (if
    (or force (not (equal last-proj-exts emamux-ghci:exts)))
    (progn ; update the extensions
      (emamux-ghci:clear-exts last-proj-exts emamux-ghci:tmux-address)
      (emamux-ghci:issue-set (exts-arg emamux-ghci:exts nil) emamux-ghci:tmux-address)

      ; update the sync state for exts
      (setq last-proj-exts emamux-ghci:exts))
    nil)
  (if
    (or force (not (equal last-proj-includes emamux-ghci:includes)))
    (progn ; update the includes
      (emamux-ghci:clear-includes emamux-ghci:tmux-address)
      (emamux-ghci:issue-set (include-arg emamux-ghci:includes)
       emamux-ghci:tmux-address)

      ; update the sync state for includes
      (setq last-proj-includes emamux-ghci:includes))
    nil))

(defun emamux-ghci:proj-load-file (fp)
  "Load the file path provided into ghci. If a previous sync hasn't
    occurred, the behavior is to sync the defined settings, otherwise
    if the previous sync differs, re sync settings."
  (emamux-ghci:proj-sync)
  (emamux-ghci:load-file fp emamux-ghci:tmux-address)
  )

(defun emamux-ghci:proj-load-buffer ()
  "resolve the path of the buffer being edited, and load that into ghci"
  (interactive)
  (emamux-ghci:proj-load-file buffer-file-name)
  (message "emamux-ghci: loaded buffer"))

(provide 'emamux-ghci)
;;; emamux-ghci.el ends here
