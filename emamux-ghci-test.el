;;; emamux-ghci-test.el --- ERT unit tests for emacs-ghci

;; Copyright (C) 2014 John P. Feltz

;; Author: John P. Feltz; <jfeltz@gmail.com>
;; Keywords: local, maint

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
;;; - Functions here modify /tmp, you've been warned.
;;; - These are sanity tests, please do not assume exhaustive testing.

;;; Code:

(require 'emamux-ghci)
(require 'ert)
(defconst test-dir (concat "/tmp/eg-test"))
(defconst address "haskell:ghci")
(defconst child-fn "Child.hs")
(make-local-variable 'test-dir )
(make-local-variable 'address)
(make-local-variable 'child-fn)

(defun test-sub-dir () (concat test-dir "/b"))
(defun child-path () (concat test-dir "/" child-fn))
(defun parent-path () (concat (test-sub-dir) "/Parent.hs"))

(defun write-str (s fp) (append-to-file s nil fp))

;;;; exts-arg ;;;;

(ert-deftest test-exts-arg-mult ()
  "concat of mult"
  (should (string= "-XA -XB" (exts-arg '("A" "B") nil)))
  (should (string= "-XNoA -XNoB" (exts-arg '("A" "B") t)))
  )
(ert-deftest test-exts-arg-single ()
  "single extension"
  (should (string= "-XA" (exts-arg '("A") nil)))
  (should (string= "-XNoA" (exts-arg '("A") t)))
  )
(ert-deftest test-exts-arg-vacous ()
  "nil case"
  (should (eq nil (exts-arg nil t)))
  )

;;;; include-arg ;;;;

(ert-deftest test-include-arg-mult ()
  "concat of mult"
  (should (string= "-ia:b" (include-arg '("a" "b"))))
  )
(ert-deftest test-include-arg-single ()
  "single include"
  (should (string= "-ia" (include-arg '("a"))))
  )
(ert-deftest test-include-arg-vacous ()
  "nil case"
  (should (eq nil (include-arg nil)))
  )

(defun fs-fixture (f) 
  (unwind-protect
    (let
      (
       (child-buf "module Child where \nimport Parent")
       (parent-buf "module Parent where")
      )
      (progn
        ; make the test FS
        (make-directory (test-sub-dir) t)
        ; write the modules to the FS 
        (write-str child-buf (child-path))
        (write-str parent-buf (parent-path))
        ; Open child & set as current buffer.
        (find-file-literally (child-path))
        (funcall f) ; test body
        )
    )
    ; Kill Child's buffer. Remarkably this is so fast
    ;   that it never seems to even open
    (kill-buffer child-fn)
    ; sleep to avoid race-condition with :ghci load, etc
    (sleep-for 2)
    ;force recursive delete (by 2nd arg)
    (delete-directory test-dir t) 
    )
  )

(defun project-fixture (f) 
  (unwind-protect
    (progn
      (setq emamux-ghci:includes '("x" "y"))
      (setq emamux-ghci:exts '("UnicodeSyntax"))
      (emamux-ghci:issue-cmd
        "\"project-fixture setup complete\""
        emamux-ghci:tmux-address)
      (funcall f)
     )
    ; clean up, effectively testing proj-sync force
    (progn
     (setq emamux-ghci:includes nil)
     (setq emamux-ghci:exts nil)
     (emamux-ghci:proj-sync t)
     (emamux-ghci:issue-cmd
       "\"project-fixture teardown complete\""
       emamux-ghci:tmux-address
      )
     )
  )
)
;(project-fixture (lambda () (message "running")))

(ert-deftest test-proj-sync ()
  "test proj-sync for established settings" 
  (project-fixture 
    (lambda ()
      (emamux-ghci:proj-sync) 

      ; change project settings 
      (setq emamux-ghci:includes '("x" "z"))
      (setq emamux-ghci:exts nil)

      ; perform resync
      (emamux-ghci:proj-sync)

      ; Result is inspected visually
      (should t)
    )
  )
)

(ert-deftest test-proj-load-file ()
  "test proj-{load-buffer,load-file}"
  (fs-fixture ; file system fixture (haskell modules/buffers/dirs)
    (lambda () 
      (project-fixture ; project settings fixture (includes/extensions)
        (lambda ()
         ; set include directory 
         (setq emamux-ghci:includes (list (test-sub-dir)))
         (emamux-ghci:proj-load-file buffer-file-name)
         ; Result is inspected visually
         (should t)
         )
      )
    )
  )
)
