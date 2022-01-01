;; Set file for custom changes
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Repos
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			             ("nongnu"  . "https://elpa.nongnu.org/nongnu/")
			             ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; diminish
(use-package diminish)

;; redirect to org config file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file "~/.emacs.d/config.org"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
