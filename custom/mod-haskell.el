(provide 'mod-haskell)

(use-package haskell-mode
    :ensure t)

(use-package hindent
    :ensure t)

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (intero-global-mode 1))
