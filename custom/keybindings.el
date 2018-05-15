(provide 'keybindings)

;; i am using key-binding specific to modes provided by evil-collection.
;; for some global bindings i am using this

(general-create-definer spc-map
  :states 'normal
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
)

(spc-map
  "/" '(counsel-ag :which-key "ag")
  "TAB" '(ivy-switch-buffer :which-key "Counsel Switch Buffer")
  "SPC" '(counsel-M-x :which-key "Counsel-M-x")
  "b" '(ibuffer :which-key "Open IBuffer")
  "B" '(ibuffer-other-window :which-key "Win Open Buffer")
  ;; "w" '(hydra-window/body :which-key "Window")
  "d" '(counsel-dired-jump :which-key "Dired"))
