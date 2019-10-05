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

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page)
  (require 'evil-collection-pdf)
  (evil-collection-pdf-setup))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;; Desktop mode                                                                
;; If we've saved the positions of our windows and so forth for this            
;; project once manually, continue to save them.  Otherwise, do not do          
;; so.                                                                          
(setq desktop-save 'if-exists)
(desktop-save-mode t)

;; Taken from https://scalameta.org/metals/docs/editors/emacs.html
;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp)
;; End of code taken from scalameta url above
