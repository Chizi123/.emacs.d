(provide 'setup-files)

;; tramp setup
(when (eq window-system 'w32)
  (setq tramp-default-method "plink"))
