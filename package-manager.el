;;; Package manager selection
;; Can chose the package manager from the default package.el or straight.el
;; There may be problems with some of them in corporate environments

;; Repos
(cond ((eq package-manager 'package)
       (require 'package)
       (when (not (boundp 'package-archives))
         (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			                      ("nongnu"  . "https://elpa.nongnu.org/nongnu/")
			                      ("melpa" . "https://melpa.org/packages/"))))
       (package-initialize)

       ;; use-package
       (unless (package-installed-p 'use-package)
         (package-refresh-contents)
         (package-install 'use-package))
       (setq package-install-upgrade-built-in t)

       (eval-when-compile
         (require 'use-package))
       (require 'use-package-ensure)
       (setq use-package-always-ensure t))
      ((eq package-manager 'straight)
       (defvar bootstrap-version)
       (let ((bootstrap-file
              (expand-file-name
               "straight/repos/straight.el/bootstrap.el"
               (or (bound-and-true-p straight-base-dir)
                  user-emacs-directory)))
             (bootstrap-version 7))
         (unless (file-exists-p bootstrap-file)
           (with-current-buffer
               (url-retrieve-synchronously
                "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
             (goto-char (point-max))
             (eval-print-last-sexp)))
         (load bootstrap-file nil 'nomessage))
       (straight-use-package 'use-package)
       (setq straight-use-package-by-default t)))
