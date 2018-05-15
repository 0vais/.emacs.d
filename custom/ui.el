(provide 'ui)

;; you  won't need any of the bar thingies
;; turn it off to save screen estate
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Taken from https://stackoverflow.com/questions/26824328/hide-change-emacs-fringe-bent-arrows-due-to-word-wrapping
;; The problem is word-wrap icons are jittery and taking too much space.
;; (define-fringe-bitmap 'right-curly-arrow
;;   []
;;   ;; [#b00000000
;;   ;;  ;; #b00000000
;;   ;;  ;; #b00000000
;;   ;;  #b00000000
;;   ;;  #b01110000
;;   ;;  #b00010000
;;   ;;  ;; #b00010000
;;   ;;  #b00000000]
;;   )
;; (define-fringe-bitmap 'left-curly-arrow []
;;   ;; [#b00000000
;;   ;;  #b00001000
;;   ;;  ;; #b00001000
;;   ;;  #b00001110
;;   ;;  #b00000000
;;   ;;  ;; #b00000000
;;   ;;  ;; #b00000000
;;   ;;  #b00000000]
;;   )

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 4
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Open in full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(size-indication-mode t)

;; Line numbers are added in an efficient way only in emacs 26
;; So there is no packages supporting relative numbers.
;; Packages like nlinum-relative dont use this efficient implementation
;; TODO add relative line numbers when some package is available and toggle between absolute and relative in
;; normal and insert evil states - as tuhdo suggested 
(global-display-line-numbers-mode)
;; fixed width to avoid flicker
(setq display-line-numbers-width-start t)
;; highlight current line
(global-hl-line-mode 1)


;; hide welcome screen
(setq inhibit-startup-screen t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))
;; THEME

;; (use-package zenburn-theme :ensure)
;; TODO find a way to take out progn and make it common as this code should be executed only once before any of the theme is installed.
;; (use-package material-theme :ensure)
;; (use-package dracula-theme :ensure)
;; (use-package darktooth-theme :ensure)
;; (use-package gruvbox-theme :ensure)
;; (use-package solarized-theme :ensure)

;;only use/load one theme at a time else they end up messing with each other.
(use-package material-theme
  :ensure
  :demand
  :init
  (progn
    (defvar my-color-themes (list 'material 'solarized-dark 'solarized-light 'gruvbox-dark-soft 'gruvbox-dark-hard 'darktooth 'dracula 'zenburn))
    (defvar theme-current '(material))
    (defvar color-theme-is-global nil) ; Initialization

    (defun my-theme-set-default ()
      (interactive)
      ;; (setq theme-current my-color-themes)
      (load-theme (car theme-current) t))

    (defun my-describe-theme () ; Show the current theme
      (interactive)
      (message "%s" (car theme-current)))

    (defun my-theme-cycle ()
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
          (setq theme-current my-color-themes))
      (load-theme (car theme-current) t)
      (message "%S" (car theme-current)))
    )
  :bind
  ("C-c t" . my-theme-cycle)
  :config
  (my-theme-set-default))


;; FONTS
;; change font to Inconsolata for better looking text
;; remember to install the font DejaVu first on Operating System(if not available)
;; TODO - Does not work in mac terminal
(setq default-frame-alist '((font . "DejaVu Sans Mono 12")))
;; TODO set italic font for italic face, since Emacs does not set italic
;; face automatically
;(set-face-attribute 'italic nil
;                    :family "Inconsolata-Italic")

;By default colors were not working in the theme(in mac) so added this line from online suggestions.
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

;; install icons. For fresh installation remember to install all fonts using M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; show color in color encodings like #FFF
;; TODO enable it in appropriate mode using add hook
;; (use-package rainbow-mode
;;   :ensure t)

;; mode-line, note that power-line does not work with macs so smart-mode-line can not work with powerline theme.
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'dark)
  :config
  (sml/setup))

;; hide file size in mod-line
(setq size-indication-mode nil)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
(setq-default mode-line-format
			  (list
			   '("%e"
				 mode-line-modified
				 evil-mode-line-tag
				 sml/pos-id-separator
				 mode-line-buffer-identification
				 sml/pos-id-separator
				 )
			   ;; line and column
			   ;; "(" ;; '%02' to set to 2 chars at least; prevents flickering
			   ;; (propertize "%02l" 'face 'font-lock-type-face)
			   ;; ","
			   ;; (propertize "%02c" 'face 'font-lock-type-face)
			   
			   ;; ") "
			   ;;mode-line-front-space
			   '("%e"
				 mode-line-mule-info
				 mode-line-client
				 mode-line-remote
				 mode-line-frame-identification
				 sml/pos-id-separator
				 mode-line-misc-info
				 smartrep-mode-line-string
				 (vc-mode vc-mode)
				 sml/pre-modes-separator
				 mode-line-modes
				 mode-line-position
				 mode-line-end-spaces
				 sml/pos-id-separator
				 )
				  
			   ))
