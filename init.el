;; Set file for custom changes
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load package manager
(setq package-manager 'straight)
(load "~/.emacs.d/package-manager.el" `noerror)

;; auto-package-update
(when (eq package-manager 'package)
  (use-package auto-package-update
    :ensure t
    :config
    (setq auto-package-update-delete-old-versions t)
    (setq auto-package-update-hide-results t)
    (setq auto-package-update-interval 7)
    (setq auto-package-update-at-time "03:00")
    (auto-package-update-maybe)))

;; diminish
(use-package diminish)

;; org-mode
(use-package org)

;; redirect to org config file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file "~/.emacs.d/config.org"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
