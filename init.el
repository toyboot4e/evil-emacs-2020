;; init.el

(setq user-full-name    "Vim 侍"
      user-mail-address "vim-samurai@gmail.com")

;; allow loading symlinks
(setq vc-follow-symlinks t)

(load-file (concat user-emacs-directory "elisp/evil.el"))
(load-file (concat user-emacs-directory "elisp/ide.el"))
(load-file (concat user-emacs-directory "elisp/keymap.el"))

