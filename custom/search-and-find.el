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

;; at present, i am putting neo-tree in this ~search-and-file.el~ file
(use-package neotree
	:ensure t
	:demand
	:config
	(with-eval-after-load 'neotree (require 'evil-collection-neotree) (evil-collection-neotree-setup)

	;; Set the neo-window-width to the current width of the
	;; neotree window, to trick neotree into resetting the
	;; width back to the actual window width.
	;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
    (setq neo-window-fixed-size nil)					  
	(eval-after-load "neotree"
		'(add-to-list 'window-size-change-functions
				  (lambda (frame)
						(let ((neo-window (neo-global--get-window)))
						  (unless (null neo-window)
	 					  (setq neo-window-width (window-width neo-window)))))))))
