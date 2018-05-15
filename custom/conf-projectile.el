(provide 'conf-projectile)

(use-package projectile :ensure t
  :init
  ;; (add-hook 'after-init-hook 'projectile-mode)
  (use-package counsel-projectile :ensure t)

  ;; use ivy
  (setq projectile-completion-system 'ivy)

  ;; make projectile usable for every directory
  (setq projectile-require-project-root nil)

  ;; cd into dir i want, including git-root
  ;; (defun cd-dwim ()
  ;;     (cd (projectile-project-root)))
  ;; (setq projectile-switch-project-action 'cd-dwim)

  :config
  (projectile-global-mode)

  :diminish global-projectile-mode "")
