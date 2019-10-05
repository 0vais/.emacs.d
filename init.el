;;; package: --- main init file for emacs config
;;; Commentary:

;; Split emacs customizations generated by emacs into separate file
(setq custom-file "~/.emacs.d/emacs-customizations.el")
(load custom-file 'noerror)


;; Add and enable MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpamarmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

;; add your modules path
(add-to-list 'load-path "~/.emacs.d/custom/")
(add-to-list 'load-path "~/.emacs.d/external/")

;; To update the package list
;; checks if there are no package archives, which should only be the case  when emacs starts first time emacs in a machine.
(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

;; Next two lines taken from https://scalameta.org/metals/docs/editors/emacs.html
;; Enable defer and ensure by default for use-package
(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package diminish
  :ensure t)

;; String manipulation library
;; used in evil-org
(use-package s
  :ensure t)

;; For ecryption - note currently i am using encryption only in org mode. if thats not the case this should be moved to appropriate place.
(require 'epa-file)
(epa-file-enable)
;; documentation says something about the invocation of pinentry, which sounds like the interface for entering the passphrase. And as the documentation says, by setting epa-pinentry-mode to 'loopback Emacs will handle querying the passphrase through minibuffer, the perfect desired behavior. Taken from https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setf epa-pinentry-mode 'loopback)

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

;; load your modules
(require 'ui)
(require 'evil-editing)
(require 'conf-which-key)
(require 'keybindings)
(require 'pairs-and-brackets)
(require 'misc)
(require 'conf-backup)
(require 'conf-ibuffer)
(require 'search-and-find)
(require 'mod-lisp)
(require 'completions-and-snippets)
(require 'mod-markdown)
(require 'mod-tex)
(require 'mod-org)
(require 'mod-haskell)
(require 'mod-scala)
(require 'vc)
(require 'conf-projectile)
