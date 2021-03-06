(provide 'evil-editing)

;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
;     mode-require-final-newline t      ; add a newline to end of file
)

(setq-default tab-width 4)

;; evil uses undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package evil
  :ensure t
  :demand
  :init
  ;; this flag is checked in evil
  ;; for evil-collection to work correctly this should be t
  ;; these flag ned not be changed even if it is required to enable mod specific keybinding from evil-collection
  ;; for that just set nil in the variable evil-collection-mode-list to nil
  ;; and then mod by mod call enabling 
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; needed by evil-collection
  ;; Finer granularity of evil - https://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims/3508
  (setq evil-want-fine-undo t)

  :config
  ;; Cursor change based on states
  ;; Cursor change does not reflect in terminal
  ;; progn can be removed
  ;; TODO i think it would be better to put it in a function and calling the function
  (progn
	;; when evil-search-module is set to evil-search (either before evil loads or with custom), different search commands are used, namely evil-ex-search* instead of evil-search*. Details: "https://github.com/jojojames/evil-collection/issues/49"
	;; its better to leave to default as otherwise "n" wont work. 
	;; (evil-select-search-module 'evil-search-module 'evil-search)
	(evil-mode 1)
	;; to disable showing state in mode-line
	;; (setq evil-mode-line-format nil)
	;; in terminal cursor does not change.
	;; (setq evil-normal-state-cursor '("green" box))
	;; (setq evil-emacs-state-cursor '("red" box))
	;; (setq evil-visual-state-cursor '("orange" box))
	;; (setq evil-insert-state-cursor '("red" bar))
	;; (setq evil-replace-state-cursor '("red" bar))
	;; (setq evil-operator-state-cursor '("red" hollow))
	;; ls command by default opens list-buffer, remap it to ibuffer.
	(evil-ex-define-cmd "ls" 'ibuffer) ;; ideally its better to make ibuffer as default in emacs rather changing evil mapping but it seems it is not working
	;; configuration to have all modes start in normal state
	;; if evil-insert-state-modes contain say org-mode and tex then org-mode and tex-mode when enetered will be in insert mode.
	(setq evil-emacs-state-modes nil)
	(setq evil-insert-state-modes nil)
	(setq evil-motion-state-modes nil)
	;; In emacs state, C-z and ESC are bound to switch to the previous state. This may not be what you want if you’ve entered emacs state from insert state, so you may want to rebind ESC to always enter normal state instead:
	(define-key evil-emacs-state-map [escape] 'evil-normal-state)

	;;Using Escape to Exit the Minibuffr
	;;Escape is used as a prefix key in some parts of emacs, so you need to rebind it to keyboard-escape-quit in certain minibuffer-related keymaps for it to always act as expected
	(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
	(define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
	(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
	(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
	(define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)))

;; this integrates commenting with text-objects
;; gcc comments out a line (takes a count)
;; gc comments out the target of a motion, e.g. gcap to comment out a paragraph (normal state) and gc to comment out the selection (visual state).
(use-package evil-commentary :ensure t
  :config (evil-commentary-mode)
  :diminish "")

(defvar my-leader "<SPC>")

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup))


(use-package evil-collection
  :after evil
  :ensure t
  :demand
  :init
  :config
  ;; enabled for all modes provided by default by evil-collection
  ;; change this if wanted to enable for specific modes
  (evil-collection-init))
