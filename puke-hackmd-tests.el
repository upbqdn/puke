;;; puke-hackmd-tests.el --- Tests for puke-hackmd -*- lexical-binding: t; -*-

;; emacs --batch -L . -l puke-hackmd-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'puke-hackmd)

(defvar puke-hackmd-tests--calls nil
  "Requests made during a test, oldest first, as (METHOD PATH BODY).")

(defvar puke-hackmd-tests--remote ""
  "The content the fake HackMD note holds.")

(defun puke-hackmd-tests--request (method path &optional body)
  "Record METHOD PATH BODY and answer like HackMD would."
  (setq puke-hackmd-tests--calls
        (append puke-hackmd-tests--calls (list (list method path body))))
  (pcase method
    ("POST" (setq puke-hackmd-tests--remote (alist-get 'content body))
            '((id . "NOTE")))
    ("GET" `((content . ,puke-hackmd-tests--remote)))
    ("PATCH" (setq puke-hackmd-tests--remote (alist-get 'content body)) nil)))

(defmacro puke-hackmd-tests--with-fake (&rest body)
  "Run BODY against a fake HackMD."
  `(let ((puke-hackmd-tests--calls nil)
         (puke-hackmd-tests--remote ""))
     (cl-letf (((symbol-function 'puke-hackmd--request)
                #'puke-hackmd-tests--request))
       ,@body)))

(defun puke-hackmd-tests--methods ()
  "Return the request methods made so far."
  (mapcar #'car puke-hackmd-tests--calls))

(ert-deftest puke-hackmd-markdown-create-then-update ()
  (puke-hackmd-tests--with-fake
   (with-temp-buffer
     (insert "# Title\n\nBody\n")
     (puke-hackmd-publish)
     (should (equal (puke-hackmd-tests--methods) '("POST")))
     ;; The record line stays local: HackMD holds the text without it.
     (should (equal puke-hackmd-tests--remote "# Title\n\nBody\n"))
     (should (equal (puke-hackmd--state)
                    (cons "NOTE" (secure-hash 'sha1 "# Title\n\nBody\n"))))
     (goto-char (point-max))
     (insert "More\n")
     (puke-hackmd-publish)
     (should (equal (puke-hackmd-tests--methods) '("POST" "GET" "PATCH")))
     (should (equal puke-hackmd-tests--remote "# Title\n\nBody\nMore\n"))
     (should (equal (cdr (puke-hackmd--state))
                    (secure-hash 'sha1 "# Title\n\nBody\nMore\n"))))))

(ert-deftest puke-hackmd-refuses-to-overwrite-web-edits ()
  (puke-hackmd-tests--with-fake
   (with-temp-buffer
     (insert "Body\n")
     (puke-hackmd-publish)
     (setq puke-hackmd-tests--remote "Edited on HackMD\n")
     (should-error (puke-hackmd-publish) :type 'user-error)
     (should-not (member "PATCH" (puke-hackmd-tests--methods)))
     (should (equal puke-hackmd-tests--remote "Edited on HackMD\n"))
     ;; The prefix argument overwrites deliberately.
     (puke-hackmd-publish t)
     (should (equal puke-hackmd-tests--remote "Body\n")))))

(ert-deftest puke-hackmd-org-exports-markdown ()
  (puke-hackmd-tests--with-fake
   (with-temp-buffer
     (org-mode)
     (insert "#+TITLE: Notes\n\n* Heading\nSome /text/.\n")
     (puke-hackmd-publish)
     (should (string-prefix-p "# Notes\n\n" puke-hackmd-tests--remote))
     (should (string-search "# Heading" puke-hackmd-tests--remote))
     (should (string-search "*text*" puke-hackmd-tests--remote))
     (should (string-match-p "^#\\+HACKMD: NOTE " (buffer-string)))
     ;; Republishing does not export the record keyword.
     (puke-hackmd-publish)
     (should-not (string-search "HACKMD" puke-hackmd-tests--remote)))))

(ert-deftest puke-hackmd-keeps-front-matter-and-property-drawer-first ()
  (puke-hackmd-tests--with-fake
   (with-temp-buffer
     (insert "---\ntitle: T\n---\n# Body\n")
     (puke-hackmd-publish)
     (should (string-prefix-p "---\ntitle: T\n---\n<!-- hackmd: NOTE " (buffer-string)))
     (should (equal puke-hackmd-tests--remote "---\ntitle: T\n---\n# Body\n")))
   (with-temp-buffer
     (org-mode)
     (insert ":PROPERTIES:\n:ID: abc\n:END:\n#+TITLE: Notes\n\nText.\n")
     (puke-hackmd-publish)
     (should (string-prefix-p ":PROPERTIES:\n:ID: abc\n:END:\n#+HACKMD: NOTE " (buffer-string)))
     (should (equal (org-entry-get (point-min) "ID") "abc")))))

;;; puke-hackmd-tests.el ends here
