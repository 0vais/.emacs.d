(provide 'mod-org)

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
			  (evil-org-set-key-theme '(textobjects insert navigation calendar additional shift todo))
			  (evil-org-agenda-set-keys)
			  ))
  ;; to add quick notes in LOGBOOK drawer
  (setq org-log-into-drawer t)
  (setq org-agenda-files (list "~/Dropbox/org/logs"))
  
  ;; make agenda to read gpg files too
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
	(setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
									org-agenda-file-regexp)))
  (setq org-todo-keywords
		(quote ((sequence "TODO(t/!)" "NEXT(n/!)" "|" "DONE(d@)")
				(sequence "SOMEDAY(s/!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@)" ))))
  
  (setq org-todo-keyword-faces
		(quote (("TODO" :foreground "red" :weight bold)
				("SOMEDAY" :foreground "yellow" :weight bold)
				("NEXT" :foreground "light slate blue" :weight bold)
				("DONE" :foreground "lime green" :weight bold)
				("WAITING" :foreground "orange" :weight bold)
				("HOLD" :foreground "magenta" :weight bold)
				("CANCELLED" :foreground "lime green" :weight bold))))
  (setq org-capture-templates
		'(("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org") "* %?\n")))

  (setq org-agenda-custom-commands
      '(("Q" . "Custom queries") ;; gives label to "Q" 
        ("Qt" "Task search" alltodo "Searches todos only in log files"
         ((org-agenda-files (file-expand-wildcards "~/Dropbox/org/logs/*.org")))) 
        ("Qn" "Notes search" search "Searches in notes files"
         ((org-agenda-files (file-expand-wildcards "~/Dropbox/org/notes/*.org"))))
        ;; ...other commands here
        )) 
  )

;; Uncomment/use the appropriate bullets to use
(use-package org-bullets
  :ensure t
  :init

  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (setq org-bullets-bullet-list '("•"))

  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (setq org-ellipsis "⤵")
  ;; (setq org-ellipsis "…")
  (setq org-ellipsis "⤷")
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(defun ovi/org-tasks-agenda ()
  (interactive)
  (setq org-agenda-files (list "~/Dropbox/org/logs"))
  (org-agenda))
(defun ovi/org-notes-agenda ()
  (setq org-agenda-files (list "~/Dropbox/org/notes"))
  (interactive)
  (org-agenda))

;; a custome function to add time manually
;; taken from https://emacs.stackexchange.com/questions/30280/how-to-conveniently-insert-a-clock-entry
;; TODO at present it inserts time at the current point. Instead it should insert the time in LOGBOOK drawer of the the nearest heading.
(defun jx/insert-custom-clock-entry ()
  (interactive)
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  (org-time-stamp-inactive)
  (org-ctrl-c-ctrl-c))

;; ;; keybindings
(my-local-leader-def
  :keymaps 'org-mode-map
  "s" '(org-schedule :which-key "Org Schedule")
  "d" '(org-deadline :which-key "Org Deadline")
  "a" '(org-add-note :which-key "Org add note.")
  
  "c c" '(jx/insert-custom-clock-entry :which-key "Insert clock custom/manual time.")
  "c e" '(org-set-effort :which-key "Insert effort estimate")
  
  "!" '(org-time-stamp-inactive :which-key "Insert inactive time")
  "." '(org-time-stamp :which-key "Insert time")
  
  
  "h i" '(org-insert-heading-after-current :which-key "Org insert heading after current.")
  "h I" '(org-insert-heading :which-key "Org insert heading.")
  "h s" '(org-insert-subheading :which-key "Org insert sub-heading.")
  
  "r s" '(org-archive-subtree :which-key "Org archive subtree")
  "r a" '(org-archive-subtree-default-with-confirmation :which-key "Org archive with confirmation")

  "i l" '(org-insert-link :which-key "Org insert link.")
  "i d" '(org-insert-drawer :which-key "Org insert drawer.")
  ":" '(org-set-tags :which-key "Org set tags.")
  )


;; org timeline is removed so to get TODOs from current buffer
;; this is an implementation of L that agenda had earlier.
;; taken from https://www.reddit.com/r/orgmode/comments/7hps9j/rip_orgtimeline/
;; (add-hook 'org-load-hook
;; 			(lambda ()
;; 			  (setq org-agenda-custom-commands
;; 					'(("L" "my view"
;; 					   ((todo
;; 						 "TODO"
;; 						 ((org-agenda-overriding-header "== TODO tasks without scheduled date ==")
;; 						  (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
;; 						  (org-agenda-prefix-format '((todo . " %1c ")))))
;; 						(agenda
;; 						 ""
;; 						 ((org-agenda-overriding-header "== Scheduled tasks ==")
;; 						  (org-agenda-span 22)
;; 						  (org-agenda-prefix-format '((agenda . " %1c %?-12t% s")))))))))))
