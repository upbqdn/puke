;;; puke-hackmd.el --- Publish Markdown and Org files to HackMD -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; M-x puke-hackmd-publish uploads the current buffer to a HackMD note:
;; Markdown as it is, Org through the built-in Markdown exporter.  The first
;; publish creates the note and records its id and a hash of the uploaded text
;; in the file (`#+HACKMD:' in Org, `<!-- hackmd: -->' in Markdown).  Later
;; publishes update that note, refusing when it was edited on HackMD since the
;; last publish unless called with a prefix argument.  Local images are not
;; uploaded.

;;; Code:

(require 'auth-source-pass)
(require 'url)

(defvar url-http-end-of-headers)
(declare-function org-export-as "ox")
(declare-function org-get-title "org")

(defgroup puke-hackmd nil
  "Publish Markdown and Org files to HackMD."
  :group 'puke
  :prefix "puke-hackmd-")

(defcustom puke-hackmd-pass-entry "api.hackmd.io"
  "The `pass' entry whose first line is the HackMD API token."
  :type 'string)

(defcustom puke-hackmd-read-permission "guest"
  "Who may read notes this package creates."
  :type '(choice (const "owner") (const "signed_in") (const "guest")))

(defconst puke-hackmd--api "https://api.hackmd.io/v1")

(defun puke-hackmd--request (method path &optional body)
  "Send METHOD to the HackMD API PATH with JSON BODY; return the parsed reply."
  (let* ((token (or (let ((inhibit-message t)) ; Keep decryption off the echo area.
                      (auth-source-pass-get 'secret puke-hackmd-pass-entry))
                    (user-error "No HackMD token in pass entry %s"
                                puke-hackmd-pass-entry)))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("Content-Type" . "application/json")))
         (url-request-data
          (and body (encode-coding-string (json-serialize body) 'utf-8)))
         (buffer (or (url-retrieve-synchronously
                      (concat puke-hackmd--api path) t t 30)
                     (error "HackMD %s %s: no response" method path))))
    (with-current-buffer buffer
      (unwind-protect
          (let ((status (bound-and-true-p url-http-response-status)))
            (unless (and status (<= 200 status 299))
              (error "HackMD %s %s: HTTP %s" method path status))
            (let ((text (decode-coding-string
                         (buffer-substring-no-properties
                          url-http-end-of-headers (point-max))
                         'utf-8)))
              (unless (string-blank-p text)
                (json-parse-string text :object-type 'alist
                                   :null-object nil))))
        (kill-buffer buffer)))))

(defun puke-hackmd--marker-regexp ()
  "Match the line recording the note id and hash in this buffer."
  ;; No trailing `$': the Markdown pattern is extended with its newline when stripped.
  (if (derived-mode-p 'org-mode)
      "^#\\+HACKMD: \\(\\S-+\\) \\(\\S-+\\)"
    "^<!-- hackmd: \\(\\S-+\\) \\(\\S-+\\) -->"))

(defun puke-hackmd--state ()
  "Return (ID . HASH) recorded in this buffer, or nil before the first publish."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (puke-hackmd--marker-regexp) nil t)
      (cons (match-string-no-properties 1) (match-string-no-properties 2)))))

(defun puke-hackmd--record (id hash)
  "Record note ID and the HASH of its uploaded text in this buffer."
  (let ((line (format (if (derived-mode-p 'org-mode)
                          "#+HACKMD: %s %s"
                        "<!-- hackmd: %s %s -->")
                      id hash)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (puke-hackmd--marker-regexp) nil t)
          (replace-match line t t)
        ;; Front matter and Org's file-level property drawer (org-roam's ID) must stay first.
        (let* ((org (derived-mode-p 'org-mode))
               (end (and (looking-at (if org ":PROPERTIES:$" "---$"))
                         (save-excursion
                           (forward-line)
                           (re-search-forward
                            (if org "^:END:$" "^\\(---\\|\\.\\.\\.\\)$") nil t)))))
          (when end
            (goto-char end)
            (forward-line)))
        (unless (bolp) (insert "\n"))
        (insert line "\n")))))

(defun puke-hackmd--content ()
  "Return the text the note should hold."
  (if (derived-mode-p 'org-mode)
      (progn
        (require 'ox-md)
        ;; Unknown keywords such as #+HACKMD are not exported.
        (let ((title (org-get-title))
              (body (org-export-as 'md nil nil nil '(:with-toc nil))))
          (if title (concat "# " title "\n\n" body) body)))
    (replace-regexp-in-string
     (concat (puke-hackmd--marker-regexp) "\n?") ""
     (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun puke-hackmd-publish (&optional force)
  "Publish this buffer to its HackMD note, creating the note the first time.
Refuse to overwrite edits made on HackMD since the last publish unless FORCE
\(the prefix argument) is non-nil."
  (interactive "P")
  (let* ((content (puke-hackmd--content))
         (hash (secure-hash 'sha1 content))
         (state (puke-hackmd--state))
         (id (car state)))
    (if (not id)
        (setq id (or (alist-get 'id (puke-hackmd--request
                                     "POST" "/notes"
                                     `((content . ,content)
                                       (readPermission . ,puke-hackmd-read-permission)
                                       (writePermission . "owner"))))
                     (error "HackMD created a note but returned no id")))
      (let ((remote (alist-get 'content
                               (puke-hackmd--request "GET" (concat "/notes/" id)))))
        (unless (or force (equal (secure-hash 'sha1 (or remote "")) (cdr state)))
          (user-error "Note %s was edited on HackMD since the last publish; \
C-u M-x puke-hackmd-publish overwrites it" id)))
      (puke-hackmd--request "PATCH" (concat "/notes/" id) `((content . ,content))))
    (puke-hackmd--record id hash)
    (message "Published to https://hackmd.io/%s" id)))

(provide 'puke-hackmd)
;;; puke-hackmd.el ends here
