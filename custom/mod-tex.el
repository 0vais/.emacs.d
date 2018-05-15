(provide 'mod-tex)

(use-package tex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))
