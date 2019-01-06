;; adding modules to load path
;; (add-to-list 'load-path "~/.emacs.d/custom/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/")

;; load your modules
;; (require 'setup-applications)
;; (require 'setup-communication)
;; (require 'setup-convenience)
;; (require 'setup-data)
;; (require 'setup-development)
;; (require 'setup-editing)
;; (require 'setup-environment)
;; (require 'setup-external)
;; (require 'setup-faces)
;; (require 'setup-files)
;; (require 'setup-help)
;; (require 'setup-programming)
;; (require 'setup-text)
;; (require 'setup-local)

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

;; CEDET
;; (use-package semantic
;;   :config
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (global-semantic-idle-summary-mode 1)
;;   (semantic-mode 1))

;; (use-package ede
;;   :config
;;   (global-ede-mode t))

;; (setq
;;  ;; use gdb-many-windows by default
;;  gdb-many-windows t

;;  ;; Non-nil means display source file containing the main routine at startup
;;  gdb-show-main t)

;; ;; undo-tree
;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (global-undo-tree-mode))

;; ;; volatile highlights
;; (use-package volatile-highlights
;;   :ensure t
;;   :config
;;   (volatile-highlights-mode t))

;; ;; yasnippet
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))

;; ;; ggtags
;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda
;; 	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;; 		(ggtags-mode 1)))))

;; ;; workgroups2
;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (workgroups-mode 1))

;; ;; smartparens
;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :config
;;   (progn
;;     (require 'smartparens-config)
;;     (smartparens-global-mode 1)))

;; ;; clean-aindent-mode
;; (use-package clean-aindent-mode
;;   :ensure t
;;   :hook prog-mode)

;; ;; company config
;; (use-package company
;;   :ensure t
;;   :init (global-company-mode)
;;   :config
;;   (add-hook 'c-mode-common-hook
;; 	    (lambda ()
;; 	      (define-key c-mode-base-map  [(tab)] 'company-complete))))

;; (use-package company-c-headers
;;   :ensure t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-c-headers))

;; (use-package company-math
;;   :ensure t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-math-symbols-unicode))

;; ;; projectile config
;; (use-package projectile
;;   :ensure t
;;   :bind (("C-c p" . projectile-command-map))
;;   :config
;;   (projectile-global-mode)
;;   (setq projectile-completion-system 'helm)
;;   (setq projectile-indexing-method 'alien))

;; ;; magit config
;; (use-package magit
;;   :ensure t
;;   :commands magit-get-top-dir
;;   :bind ("C-x g" . magit-status)
;;   :init
;;   (progn
;;     ;; make magit status go full-screen but remember previous window
;;     ;; settings
;;     ;; from: http://whattheemacsd.com/setup-magit.el-01.html
;;     (defadvice magit-status (around magit-fullscreen activate)
;;       (window-configuration-to-register :magit-fullscreen)
;;       ad-do-it
;;       (delete-other-windows))

;;     ;; Close popup when commiting - this stops the commit window
;;     ;; hanging around
;;     ;; From: http://git.io/rPBE0Q
;;     (defadvice git-commit-commit (after delete-window activate)
;;       (delete-window))

;;     (defadvice git-commit-abort (after delete-window activate)
;;       (delete-window))

;;   :config
;;   (progn
;;     ;; restore previously hidden windows
;;     (defadvice magit-quit-window (around magit-restore-screen activate)
;;       (let ((current-mode major-mode))
;;         ad-do-it
;;         ;; we only want to jump to register when the last seen buffer
;;         ;; was a magit-status buffer.
;;         (when (eq 'magit-status-mode current-mode)
;;           (jump-to-register :magit-fullscreen)))))

;;   ;; magit settings
;;   (setq
;;    ;; don't put "origin-" in front of new branch names by default
;;    magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
;;    ;; open magit status in same window as current buffer
;;    magit-status-buffer-switch-function 'switch-to-buffer
;;    ;; highlight word/letter changes in hunk diffs
;;    magit-diff-refine-hunk t
;;    ;; ask me if I want to include a revision when rewriting
;;    magit-rewrite-inclusive 'ask
;;    ;; ask me to save buffers
;;    magit-save-some-buffers t
;;    ;; pop the process buffer if we're taking a while to complete
;;    magit-process-popup-time 10
;;    ;; ask me if I want a tracking upstream
;;    magit-set-upstream-on-push 'askifnotset
;;    )))

;; ;; flycheck
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :after flycheck
;;   :config
;;   (flycheck-pos-tip-mode))

;; (use-package flycheck-clang-analyzer
;;   :ensure t
;;   :after flycheck
;;   :config
;;   (flycheck-clang-analyzer-setup))

;; ;; nyan mode
;; (use-package nyan-mode
;;   :if window-system
;;   :ensure t
;;   :config
;;   (nyan-mode))

;; ;; semantic refactor
;; (use-package srefactor
;;   :ensure t
;;   :bind (("M-RET o" . 'srefactor-lisp-one-line)
;; 	 ("M-RET m" . 'srefactor-lisp-format-sexp)
;; 	 ("M-RET d" . 'srefactor-lisp-format-defun)
;; 	 ("M-RET b" . 'srefactor-lisp-format-buffer)
;; 	 :map c-mode-base-map
;; 	      ("M-RET" . 'srefactor-refactor-at-point)
;; 	      :map c++-mode-map
;; 	      ("M-RET" . 'srefactor-refactor-at-point)))

;; ;; which-key
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode)
;;   (which-key-setup-side-window-bottom))

;; ;; x86 lookup
;; (use-package x86-lookup
;;   :ensure t
;;   :init
;;   (setq x86-lookup-pdf "D:/Coding/x86-instructions.pdf")
;;   :bind ("C-h x" . x86-lookup))

;; ;; org-bullets
;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ;; pdf-tools
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

;; ;; org
;; (use-package org
;;   :ensure t
;;   :config
;;   (setq org-src-tab-acts-natively t))

;; ;; tex/AUCTex
;; (use-package tex
;;   :ensure auctex
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq doc-view-ghostscript-program "c:/msys64/mingw64/bin/gswin32c.exe")
;;   (setq preview-gs-command "c:/msys64/mingw64/bin/gs.exe"))

;; ;; latex-preview-pane
;; (use-package latex-preview-pane
;;   :ensure t
;;   :config
;;   (latex-preview-pane-enable))

;; ;; plantuml
;; (use-package plantuml-mode
;;   :ensure t
;;   :init
;;   (setq plantuml-jar-path "c:/ProgramData/chocolatey/lib/plantuml/tools/plantuml.jar"))

;; (use-package keyfreq
;;   :ensure t
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("c:/Users/joelg/.emacs.d")))
 '(package-selected-packages
   (quote
    (diminish keyfreq zenburn-theme yasnippet x86-lookup workgroups2 which-key volatile-highlights use-package undo-tree srefactor smartparens racket-mode popwin plantuml-mode pdf-tools org-bullets org nyan-mode magit latex-preview-pane helm-projectile ggtags flycheck-pos-tip flycheck-clang-analyzer company-math company-c-headers clean-aindent-mode auto-package-update auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
