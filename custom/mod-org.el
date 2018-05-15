(provide 'mod-org)

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
			  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
			  (evil-org-agenda-set-keys)
			  )))

;; Uncomment/use the appropriate bullets to use
(use-package org-bullets
  :ensure t
  :init

  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (setq org-bullets-bullet-list '("•"))

  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (setq org-ellipsis "⤵")
  (setq org-ellipsis "…")

  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))
