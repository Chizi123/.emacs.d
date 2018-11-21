(provide 'setup-environment)

(setq inhibit-startup-screen t)

(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60     ; save every minute
      )

(winner-mode 1)

(column-number-mode 1)

;; remove backup files
(when (eq system-type 'windows-nt)
  (defadvice backup-buffer (after my-backup-make-hidden activate)
    (let ((backup-file-name (make-backup-file-name (buffer-file-name))))
      (when (file-exists-p backup-file-name)
	(call-process "attrib.exe" nil nil nil "+I" "+H" backup-file-name)))))
