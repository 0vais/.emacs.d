(provide 'misc)

;; (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

;; To start the mode type clm/command-log-mode
;; To open the command buffer type /clm/open-command-log
(use-package command-log-mode
  :ensure t
  :demand)

;; highlighting fixme and todo
(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))
