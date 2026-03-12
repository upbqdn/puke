;;; puke.el --- Publish Using Knowledge from Emacs -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (org-roam "2.3.1") (ox-hugo "0.12.1"))

;;; Code:

(require 'ox-hugo)

(defgroup puke nil
  "Publish org-roam notes to Hugo."
  :group 'comm
  :prefix "puke-")

(defcustom puke-hugo-base-dir "~/marek.onl/"
  "Hugo site directory."
  :type 'directory)

(defcustom puke-deploy-host "marek:/var/www/marek.onl"
  "Rsync destination for deployment."
  :type 'string)

(defconst puke-z-base-32-alphabet "ybndrfg8ejkmcpqxot1uwisza345h769")

(defun puke-encode-z-base-32 (n)
  "Encode non-negative integer N as a z-base-32 string."
  (if (zerop n)
      "y"
    (let ((result ""))
      (while (> n 0)
        (setq result (concat (string (aref puke-z-base-32-alphabet (% n 32))) result))
        (setq n (/ n 32)))
      result)))

(defun puke-counter (delta)
  "Adjust counter by DELTA, save, and return new value."
  (let* ((file (expand-file-name "id-counter" org-roam-directory))
         (n (+ delta (with-temp-buffer
                       (insert-file-contents file)
                       (string-to-number (string-trim (buffer-string)))))))
    (when (< n 0)
      (user-error "Counter would go negative"))
    (with-temp-file file
      (insert (number-to-string n)))
    n))

;;;###autoload
(defun puke-insert-id ()
  "Insert the next z-base-32 ID at point."
  (interactive)
  (insert (puke-encode-z-base-32 (puke-counter 1))))

;;;###autoload
(defun puke-release-id ()
  "Decrement the counter to reclaim an unused ID."
  (interactive)
  (message "Next ID: %s" (puke-encode-z-base-32 (+ (puke-counter -1) 1))))

(defun puke-deploy ()
  "Deploy Hugo notes asynchronously."
  (message "Deploying notes.")
  (let ((default-directory puke-hugo-base-dir))
    (async-shell-command
     (format "rsync -a %s/data/ static/data \
&& (cd themes/statine && npx @tailwindcss/cli -i assets/css/main.css -o assets/css/style.css) \
&& hugo \
&& npx pagefind --site public \
&& rsync -az --delete public/ %s"
             org-roam-directory
             puke-deploy-host)
     "*puke-deploy*")))

;;;###autoload
(defun puke-publish-note ()
  "Publish the current org-roam note."
  (interactive)
  (let ((org-hugo-base-dir puke-hugo-base-dir)
        (org-hugo-section ""))
    (org-hugo-export-wim-to-md t))
  (puke-deploy)
  (message "%s was published." (buffer-name)))

;;;###autoload
(defun puke-rebuild-notes ()
  "Rebuild all org-roam notes."
  (interactive)
  (let ((org-hugo-base-dir puke-hugo-base-dir)
        (org-hugo-section ""))
    (shell-command (format "rm %scontent/*" puke-hugo-base-dir))
    (let ((user-buffers (buffer-list)))
      (mapc (lambda (note)
              (with-current-buffer (find-file-noselect note)
                (org-hugo-export-wim-to-md t)))
            (org-roam-list-files))
      (mapc (lambda (buffer)
              (unless (member buffer user-buffers)
                (kill-buffer buffer)))
            (buffer-list))))
  (puke-deploy)
  (message "Notes were published."))

(defconst puke-anchor-types
  '(("def" . ("def" . "Definition"))
    ("rem" . ("rem" . "Remark"))
    ("exp" . ("exp" . "Example"))
    ("prp" . ("prp" . "Proposition"))
    ("thm" . ("thm" . "Theorem"))
    ("alg" . ("alg" . "Algorithm"))
    ("eqn" . ("eqn" . "Equation"))
    ("fig" . ("fig" . "Figure"))
    ("tab" . ("tab" . "Table"))
    ("lst" . ("lst" . "Listing"))
    ("etu" . ("etu" . "Etumon")))
  "Alist of anchor types.
Each entry is (TYPE . (PREFIX . LABEL)) where TYPE is used for the
block wrapper, PREFIX for the anchor, and LABEL for display.")

;;;###autoload
(defun puke-insert-anchor ()
  "Insert a labeled anchor with its block wrapper at point.
Prompts for the block type, allocates the next z-base-32 ID, and
inserts the full scaffold."
  (interactive)
  (let* ((type (completing-read "Type: " puke-anchor-types nil t))
         (entry (cdr (assoc type puke-anchor-types)))
         (prefix (car entry))
         (label (cdr entry))
         (id (puke-encode-z-base-32 (puke-counter 1)))
         (anchor (format ".%s-%s" prefix id))
         (tag (format "%s %s" label id)))
    (insert (format "#+BEGIN_%s\n<<%s>> *[[%s][%s]]*. \n#+END_%s"
                    type anchor anchor tag type))
    (forward-line -1)
    (end-of-line)))

(defun puke--collect-anchors ()
  "Collect all anchors from org files in `org-roam-directory'.
Return a list of (DISPLAY . (FILE ANCHOR DESCRIPTION)) entries."
  (let ((files (directory-files org-roam-directory t "\\.org\\'"))
        (anchor-re (concat "^<<\\(\\.[a-z]+-[a-z0-9]+\\)>>"
                           " \\*\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]\\*"
                           "\\.?\\(.*\\)"))
        results)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((title (when (re-search-forward "^#\\+title: \\(.+\\)" nil t)
                       (match-string 1))))
          (goto-char (point-min))
          (while (re-search-forward anchor-re nil t)
            (let* ((anchor (match-string 1))
                   (label (match-string 2))
                   (desc (string-trim (match-string 3)))
                   (desc (replace-regexp-in-string
                          "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" desc))
                   (desc (replace-regexp-in-string "\\\\(\\|\\\\)" "" desc))
                   (desc (truncate-string-to-width desc 60))
                   (display (format "%s | %s%s"
                                    (or title (file-name-base file))
                                    label
                                    (if (string-empty-p desc) ""
                                      (concat " — " desc)))))
              (push (cons display (list (file-name-nondirectory file)
                                        anchor desc))
                    results))))))
    (nreverse results)))

;;;###autoload
(defun puke-insert-ref ()
  "Search anchors across org-roam and insert a cross-reference at point."
  (interactive)
  (let* ((candidates (puke--collect-anchors))
         (chosen (completing-read "Anchor: " candidates nil t))
         (entry (cdr (assoc chosen candidates)))
         (file (nth 0 entry))
         (anchor (nth 1 entry))
         (desc (nth 2 entry))
         (current-file (and buffer-file-name
                            (file-name-nondirectory buffer-file-name)))
         (default-text (if (string-empty-p desc)
                           anchor
                         (substring-no-properties desc 0
                                                  (min (length desc) 40))))
         (link-text (read-string (format "Link text (default %s): " default-text)
                                 nil nil default-text))
         (same-file (string-equal file current-file)))
    (insert (if same-file
                (format "[[%s][%s]]" anchor link-text)
              (format "[[file:%s::%s][%s]]" file anchor link-text)))))

(provide 'puke)
;;; puke.el ends here
