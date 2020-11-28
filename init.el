;; init.el

(setq user-full-name    "Vim 侍"
      user-mail-address "vim-samurai@gmail.com")

(when (version< emacs-version "27.1") (error "Update your Emacs!"))

;; read symlinks
(setq vc-follow-symlinks t)

;; list files to load
(setq toy/init-files
      '("elisp/gc.el"       ;; GC settings for startup speed
        "elisp/evil.el"     ;; Evil
        "elisp/ide.el"      ;; Intelligence
        "elisp/hydra.el"    ;; Hydra
        "elisp/keymap.el"   ;; Key mappings
        ))

;; load them all
(dolist (x toy/init-files)
    (load-file (concat user-emacs-directory x)))

;; start Emacs with only one window
(add-hook 'window-setup-hook #'delete-other-windows)

