(provide 'mod-markdown)


(defconst liquid-tags-font-lock-keywords
  `((,(regexp-opt '("highlight" "endhighlight" "linenos" "if" "elsif" "else" "case" "when" "unless" "for" "in") 'words) . font-lock-keyword-face))
  "Highlighting Liquid tags")

;; ###autoload
(define-derived-mode liquid-tags-mode fundamental-mode "Liquid"
  "Major mode for Liquid Tags"
  (set (make-local-variable 'font-lock-defaults) '(liquid-tags-font-lock-keywords nil t)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (use-package yaml-mode :ensure t)
  ;; evil-markdown not present in elpa so i saved it in exernal dir.
  (require 'evil-markdown))

  ;; (mmm-add-classes '((markdown-java
  ;;                         :submode java-mode
  ;;                         :face mmm-declaration-submode-face
  ;;                         :front "^{%\s*highlight\s* java \s*\\w*\s*%}\n"
  ;;                         :back "^{%\s*endhighlight\s*%}")))

 ;; (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)

(defun ovi-mmm-markdown-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "{%\s*highlight\s*" lang "\s*\\w*\s*%}\n"))
        (back  "{%\s*endhighlight\s*%}"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

(defun ovi-mmm-markdown-regex-class (lang st-regex end-regex include &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front st-regex)
        (back end-regex))
    (if include
		(mmm-add-classes (list (list class :submode submode :front front :back back :include-front t :include-back t)))
		(mmm-add-classes (list (list class :submode submode :front front :back back))))
    (mmm-add-mode-ext-class 'markdown-mode nil class))) 


(use-package mmm-mode
  :init
  (setq mmm-global-mode 'maybe)
  :ensure t
  ;; :defer t
  :hook (markdown-mode . mmm-mode)
  :config
  (require 'liquid-mode)
  ;; disable grey backgrounds of code blocks - it is sometimes useful to check how mmm-mode is taking the regions.
  (setq mmm-submode-decoration-level 0)

  ;; Mode names that derive directly from the language name
  (mapc 'ovi-mmm-markdown-class
		'("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "ruby" "sql" "stata" "xml" "scheme" "haskell" "scala"))
  ;; highlight new-code blocks when emacs is idle
  (setq mmm-parse-when-idle 't)

  ;; Mode names that differ from the language name
  (ovi-mmm-markdown-class "fortran" 'f90-mode)
  (ovi-mmm-markdown-class "perl" 'cperl-mode)
  (ovi-mmm-markdown-class "shell" 'shell-script-mode)
  (ovi-mmm-markdown-class "java" 'java-mode)

  (ovi-mmm-markdown-regex-class  "yaml" "\\`---\n" "[^\\`]---\n" nil)
  (ovi-mmm-markdown-regex-class "latex" "\\$\\$\\\\," "\\\\,\\$\\$" t)
  (ovi-mmm-markdown-regex-class "liquid-tags" "{%" "%}" nil)
  )
