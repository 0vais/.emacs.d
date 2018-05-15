(provide 'pairs-and-brackets)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; highlighiting paranthesis
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(use-package smartparens
  :init
  (setq sp-show-pair-delay 0.1
        ;; sp-show-pair-from-inside t
		)
  :ensure t
  :diminish ""
  :config
  (use-package smartparens-config
	:config
	(progn
	  (show-smartparens-global-mode t)
	  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
	  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
	  )))

(use-package evil-smartparens
  :ensure t
  :after smartparens)

;; highlight inner brackets with different colors.
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
