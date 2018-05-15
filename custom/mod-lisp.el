(provide 'mod-lisp)

;; This provides modal like key bindings at special points in insert, emacs states as this package is not aware of evil so it gives vim like binding at special points like (or ). It does not work in normal state for that we use lispyville 
(use-package
  lispy
  :diminish ""
  :ensure t
  ;; no matter how hard i tried to defer loading lisp packages but they just load. I think it is because emacs-lisp-mode is always loaded and all the plugins i am configured
  ;; depends on this emacs-lisp-mode for delayed loading.
  ;; initially it made me think that i do not understand how delayed-loading works by use-package but i was able to do delayed loading in markdown mode.
  :defer t
  :hook (emacs-lisp-mode . lispy-mode)
  :config

  (add-hook
   'emacs-lisp-mode-hook 
   (lambda()
	 (setq mode-name "Elisp")))

  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

  (use-package
	lispyville
	:diminish ""
	:ensure t
	:hook (lispy-mode . lispyville-mode)
	:config
	;; no safety - will probably turn it on but its preventing me from editing may be i need to learn more to do it in a way which wont require turning these flags off.
	(setq lispy-safe-delete nil
		  lispy-safe-copy nil
		  lispy-safe-paste nil
		  ;; lispy-safe-actions-no-pull-delimiters-into-comments nil
		  )
	)

  (use-package highlight-quoted
	:ensure t
	:hook (emacs-lisp-mode . highlight-quoted-mode)
	)
  
  ;; highlight-defined is an Emacs minor mode that highlights defined Emacs Lisp symbols in source code.
  ;; Currently it recognizes Lisp function, built-in function, macro, face and variable names.
  (use-package highlight-defined
	:ensure t
	:hook (emacs-lisp-mode . highlight-defined-mode)
	))

;; Creating this function for scheme as I have not defined explicit file for scheme mode
(defun xscheme ()
      "Loads xscheme and runs a scheme process in the current buffer."
      (interactive)
      (load-library "xscheme")
      (xscheme-start "scheme -emacs"
                     (buffer-name)
                     (buffer-name)))
