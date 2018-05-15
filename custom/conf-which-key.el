(provide 'conf-which-key)

;; help for keybindings
(use-package which-key :ensure t
  :demand
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode)

  ;; Highlight certain commands
  (defface ovi/which-key-highlight-2-face
    '((t . (:inherit which-key-command-description-face :foreground "indian red")))
    "Another face for highlighting commands in `which-key'.")
  
  (defface ovi/which-key-highlight-3-face
    '((t . (:inherit which-key-command-description-face :foreground "DarkOrange3")))
    "Another face for highlighting commands in `which-key'.")
  
  (setq which-key-highlighted-command-list
		'(("\\`counsel-" . ovi/which-key-highlight-2-face)
          ;; Highlight using the default `which-key-highlighted-command-face'
          "\\`describe-"
          "\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))

  :diminish "")
