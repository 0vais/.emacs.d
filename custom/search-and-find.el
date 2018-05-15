(provide 'search-and-find)

;; counsel installs ivy and swiper 
(use-package counsel
  :ensure t
  ;; :diminish ivy-mode
  :config
  (global-set-key "\C-s" 'swiper)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (with-eval-after-load 'ivy (require 'evil-collection-ivy) (evil-collection-ivy-setup)))

;; For fuzzy search "https://oremacs.com/2016/01/06/ivy-flx/"
;; now when we search using ivy - eg M-x for command - it fuzzy search for the matches
(use-package flx
  :ensure t
  :demand
  :config
  (setq ivy-re-builders-alist
		'((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))
