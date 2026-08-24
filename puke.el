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

(defcustom puke-deploy-host "marek.onl:/var/www/marek.onl"
  "Rsync destination for deployment."
  :type 'string)

(defcustom puke-pagefind-version "1.5.2"
  "Pagefind release used to build the search index.

A client bundle only understands an index built by its own release, and nginx
serves the bundle with a 7-day `expires' and no revalidation.  Bumping this is
safe because `puke-bundle-directory' puts the version in the bundle URL, so a
browser holding a cached bundle is never asked to read another release's
index; it simply stops requesting the old URL.  That invariant rests on two
things, and a bump is only safe while both hold:

  - the theme derives its bundle URLs from HUGO_PAGEFIND_VERSION, which the
    deploy exports (see the pagefind-dir.html partial); and
  - the deploy's rsync protects bundle directories from --delete, so pages
    served before the bump keep finding the index they were built against.

Note that serving `expires -1' for the bundle would not make an unversioned
bump safe: responses already cached carry their original max-age and are not
revalidated until it lapses."
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

(defconst puke-deploy-buffer "*puke-deploy*"
  "Buffer collecting the output of the most recent deploy.")

(defun puke-bundle-directory ()
  "Return the site-relative directory that holds the pagefind bundle.
The name carries the version, so a bump changes every bundle URL and no
browser can pair a cached client with an index built by another release."
  (format "pagefind-%s" puke-pagefind-version))

(defun puke--deploy-command ()
  "Return the shell pipeline that builds the site and rsyncs it out."
  (mapconcat
   #'identity
   (list
    (format "rsync -a %s static/data"
            (shell-quote-argument (expand-file-name "data/" org-roam-directory)))
    "(cd themes/statine && npx @tailwindcss/cli -i assets/css/main.css -o assets/css/style.css)"
    ;; The theme reads HUGO_PAGEFIND_VERSION to build the bundle URLs, so it
    ;; and the pagefind run below agree by construction.
    (format "HUGO_PAGEFIND_VERSION=%s hugo"
            (shell-quote-argument puke-pagefind-version))
    (format "npx pagefind@%s --site public --output-subdir %s"
            (shell-quote-argument puke-pagefind-version)
            (shell-quote-argument (puke-bundle-directory)))
    ;; --delay-updates puts every file in place at once, so the site never
    ;; serves HTML that points at a bundle which has not arrived yet.  The
    ;; protect filters keep --delete away from bundles of other versions:
    ;; pages served before a bump keep fetching index shards from the URL
    ;; they were built against, and open tabs go on working.  Pruning those
    ;; directories is a separate, deliberate act.
    (format "rsync -az --delete --delay-updates %s %s public/ %s"
            (shell-quote-argument "--filter=P /pagefind/")
            (shell-quote-argument "--filter=P /pagefind-*/")
            (shell-quote-argument puke-deploy-host)))
   " && "))

(defun puke--deploy-sentinel (process _event)
  "Report how deploy PROCESS finished."
  (when (memq (process-status process) '(exit signal))
    (let ((status (process-exit-status process)))
      (if (eq status 0)
          (message "%s" (process-get process 'puke-success))
        (message "Deploy failed (exit %s).  See %s" status puke-deploy-buffer)
        (display-buffer (process-buffer process))))))

;;;###autoload
(defun puke-deploy (&optional success-message)
  "Build the site and rsync it to `puke-deploy-host'.
Runs asynchronously in `puke-deploy-buffer'.  SUCCESS-MESSAGE is shown
once the pipeline exits cleanly, its exit status when it does not.  The
steps are chained with `&&', so a failure short-circuits before rsync
and leaves the deployed site as it was."
  (interactive)
  (let ((default-directory puke-hugo-base-dir)
        (buffer (get-buffer-create puke-deploy-buffer)))
    (when (process-live-p (get-buffer-process buffer))
      (user-error "A deploy is already running"))
    (with-current-buffer buffer
      ;; A buffer left over from `async-shell-command' carries read-only
      ;; comint text, so erasing it needs the override.
      (let ((inhibit-read-only t))
        (erase-buffer))
      (fundamental-mode))
    (let ((process (start-process-shell-command
                    "puke-deploy" buffer (puke--deploy-command))))
      (process-put process 'puke-success (or success-message "Site deployed."))
      (set-process-sentinel process #'puke--deploy-sentinel)
      (display-buffer buffer)
      (message "Deploying.")
      process)))

;;;###autoload
(defun puke-publish-note ()
  "Export the current org-roam note and deploy the site."
  (interactive)
  (let ((org-hugo-base-dir puke-hugo-base-dir)
        (org-hugo-section ""))
    (org-hugo-export-wim-to-md t))
  (puke-deploy (format "%s was published." (buffer-name))))

(defun puke--clear-exported-content ()
  "Delete what ox-hugo has written to the site's content directory."
  (let ((content (expand-file-name "content" puke-hugo-base-dir)))
    (when (file-directory-p content)
      (dolist (file (directory-files content t directory-files-no-dot-files-regexp))
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))))))

;;;###autoload
(defun puke-rebuild-notes ()
  "Re-export every org-roam note and deploy the site."
  (interactive)
  (let ((org-hugo-base-dir puke-hugo-base-dir)
        (org-hugo-section ""))
    (puke--clear-exported-content)
    (let ((user-buffers (buffer-list)))
      (mapc (lambda (note)
              (with-current-buffer (find-file-noselect note)
                (org-hugo-export-wim-to-md t)))
            (org-roam-list-files))
      (mapc (lambda (buffer)
              (unless (member buffer user-buffers)
                (kill-buffer buffer)))
            (buffer-list))))
  (puke-deploy "Notes were published."))

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
    ("etu" . ("etu" . "Etumon"))
    ("prf" . ("prf" . "Proof")))
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
