(provide 'conf-projectile)

(use-package projectile :ensure t
  :init

  ;; make projectile usable for every directory
  (setq projectile-require-project-root nil)

  :config
  ;; use ivy
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  (projectile-global-mode)

  ;; :diminish global-projectile-mode ""
  )

;; (use-package counsel-projectile
;;   :ensure t
;;   :after (counsel projectile)
;;   :config (progn
;;             ;; (setq projectile-switch-project-action 'counsel-projectile)
;;             (counsel-projectile-mode)))
