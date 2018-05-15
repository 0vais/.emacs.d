(provide 'conf-backup)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 5   ; how many of the newest versions to keep
    kept-old-versions 2    )
(setq vc-make-backup-files t) ;Make backups of files, even when they're in version control
(setq auto-save-default nil)  ;stop creating those #auto-save# files
