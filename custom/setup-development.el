(provide 'setup-development)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Start garbage collection every 100MB to improve Emacs performance
(setq gc-cons-threshold 100000000)
