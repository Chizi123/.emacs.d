;; Repos
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; diminish
(use-package diminish
  :ensure t)

;; redirect to org config file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("c:/Users/joelg/.emacs.d")))
 '(package-selected-packages
   (quote
    (csv-mode magit minesweeper speed-type spray fireplace powershell company-auctex company-bibtex company-irony-c-headers company-reftex gnuplot-mode gnuplot matlab-mode diminish keyfreq zenburn-theme yasnippet x86-lookup workgroups2 which-key volatile-highlights use-package undo-tree srefactor smartparens racket-mode popwin plantuml-mode pdf-tools org-bullets nyan-mode latex-preview-pane helm-projectile ggtags flycheck-pos-tip flycheck-clang-analyzer company-math company-c-headers clean-aindent-mode auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
