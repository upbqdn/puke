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

(provide 'puke)
;;; puke.el ends here
