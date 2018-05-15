(provide 'conf-ibuffer)

(general-imap "j"
              (general-key-dispatch 'self-insert-command
                "k" 'evil-normal-state))

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq-default ibuffer-show-empty-filter-groups nil)
(setq ibuffer-default-sorting-mode 'recency)

(setq ibuffer-filter-group-name-face 'org-level-4)
(setq ibuffer-deletion-face 'font-lock-warning-face)

(use-package ibuffer-vc
  :ensure t
  :demand
  :init
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(with-eval-after-load 'ibuffer
  (require 'ibuffer-vc)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))))
