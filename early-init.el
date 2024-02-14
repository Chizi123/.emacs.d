;; Disable package at startup in case of other package managers being used
(setq package-enable-at-startup nil)
(makunbound 'package-archives)
(setq-default package-manager 'package)
