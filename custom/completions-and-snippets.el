(provide 'completions-and-snippets)

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "ya")
  :config
  (yas-global-mode 1))

;; Taken from "https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/awaken.org"
(defun trishume:company-backend-with-yas (backend)
  "Add :with company-yasnippet to company BACKEND.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend)
                backend
              (list backend))
            '(:with company-yasnippet))))

(defun dgutov:company-preview-if-explicit-action-frontend (command)
  "Show completions in buffer as cycle through menu.
https://github.com/company-mode/company-mode/issues/216#issuecomment-69871034"
  (when (or (not (eq command 'post-command))
            (and
             (company-explicit-action-p)
             company-common
             (or (eq (company-call-backend 'ignore-case) 'keep-prefix)
                 (string-prefix-p company-prefix company-common))))
    (company-preview-frontend command)))

(use-package company
  :ensure t
  :defer 3
  :diminish ""
  :config
  (setq company-backends
        (mapcar #'trishume:company-backend-with-yas company-backends))
  (global-company-mode))
  
  ;; setting idle delay too low can cause a lot of lag depending on the backend
  (setq company-idle-delay 0.15
		company-tooltip-limit 15
		company-minimum-prefix-length 2
		;; automatically complete after entering menu
		company-auto-complete #'company-explicit-action-p
		company-auto-complete-chars " )."
		company-show-numbers nil
		;; company-tooltip-align-annotations t
		company-frontends
		(list #'company-pseudo-tooltip-unless-just-one-frontend
              ;; #'company-echo-metadata-frontend
              #'dgutov:company-preview-if-explicit-action-frontend))

;; company fonts are not pleasing
;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(defvar noct:company-complete-then-cycle
  (general-predicate-dispatch #'company-manual-begin
    (company-explicit-action-p) #'company-complete-common-or-cycle)
  "Menu-item used to first explicitly enter company menu and then cycle.")
(general-emacs-define-key company-active-map
  "C-h" nil
  "C-w" nil
  "C-p" #'company-select-previous-or-abort
  "C-n" #'company-select-next-or-abort
  "<tab>" noct:company-complete-then-cycle
  "TAB" noct:company-complete-then-cycle)

(general-emacs-define-key company-active-map
  :predicate '(company-explicit-action-p)
  "<return>" #'company-complete-selection
  "RET" #'company-complete-selection
  "<up>" #'company-select-previous-or-abort
  "<down>" #'company-select-next-or-abort)

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (add-hook 'company-mode-hook #'company-quickhelp-mode))

;;disabled all this crap since this configuration is already working fine without any keybinding from evil-collection.
  ;;(progn
	;;(define-key yas-keymap (kbd "TAB") nil)
	;;(general-define-key :states '(normal insert emacs)
	;;				:keymaps 'yas-keymap "C-k" 'yas-next-field-or-maybe-expand)
	;;)
;; Also enable vim bindings using evil-collection. The above code enables tab completion only.
;; (with-eval-after-load 'company (require 'evil-collection-company) (evil-collection-company-setup))
