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

;; display date/time in mod-line
;; (setq display-time-day-and-date t)
;; display time in the mod-line if date is enabled(via display-time-day-and-date)
;; (display-time-mode 1)
(setq column-number-mode t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; FONTS
;; change font to Inconsolata for better looking text
;; remember to install the font DejaVu first on Operating System(if not available)
;; TODO - Does not work in mac terminal
;; (setq default-frame-alist '((font . "DejaVu Sans Mono 10")))
;; TODO set italic font for italic face, since Emacs does not set italic
;; face automatically
;(set-face-attribute 'italic nil
;                    :family "Inconsolata-Italic")

;; Taken from https://www.reddit.com/r/emacs/comments/5twcka/which_font_do_you_use/ddq3mx7?utm_source=share&utm_medium=web2x
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 10"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; specify font for chinese characters using default chinese font on linux
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

										;By default colors were not working in the theme(in mac) so added this line from online suggestions.
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

;; install icons. For fresh installation remember to install all fonts using M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; show color in color encodings like #FFF
;; TODO enable it in appropriate mode using add hook
;; (use-package rainbow-mode
;;   :ensure t)

(use-package smart-mode-line
  :demand
  :ensure t
  :config
  (setq sml/replacer-regexp-list nil))

(use-package smart-mode-line-powerline-theme
  :demand
  :ensure t
  :after smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'powerline))

;;To highlight current buffer 
;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window)) 
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#626262"))))))
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)



;; THEME

;; (use-package zenburn-theme :ensure)
;; TODO find a way to take out progn and make it common as this code should be executed only once before any of the theme is installed.
;; (use-package material-theme :ensure)
;; (use-package dracula-theme :ensure)
;; (use-package darktooth-theme :ensure)
;; (use-package gruvbox-theme :ensure)
;; (use-package solarized-theme :ensure)
;;only use/load one theme at a time else they end up messing with each other.
(use-package zenburn-theme
  :ensure
  :demand
  :init
  
  :config
  ;; (custom-theme-set-faces
  ;;  'zenburn
  ;;   `(isearch ((t (:foreground , "white" :weight bold :background , "SteelBlue"))))
  ;;  `(lazy-highlight ((t (:foreground , "white" :weight bold :background , "SteelBlue")))))
  
  (load-theme 'zenburn t)
  ;; change color for selection as it is very light in zenburn
  (set-face-attribute 'region nil :background "#808080")
  
  ;; change color for search highlight as this is also very light n zenburn
  
  (custom-set-faces
   `(isearch ((t (:foreground , "black" :weight bold :background , "#fff200"))))
   `(lazy-highlight ((t (:foreground "red" :weight bold :background "#ede110")))))
  )

