;; ------------------------------ Setting up ------------------------------

;; don't save custom variables
(setq custom-file (make-temp-file ""))

(setq make-backup-files nil        ; don't create backup~ files
      auto-save-default nil        ; don't create #autosave# files
      inhibit-startup-message t    ; don't show welcome screen
      ring-bell-function 'ignore   ; don't make beep sounds
      )

(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

(progn ;; UTF-8
    (set-charset-priority 'unicode)
    (prefer-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)
    ;; it modifies the buffer
    ;; (set-buffer-file-coding-system 'utf-8)
    ;; it requires flusing
    ;; (set-terminal-coding-system 'utf-8)
    (set-clipboard-coding-system 'utf-8)
    (set-file-name-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (modify-coding-system-alist 'process "*" 'utf-8))

;; ------------------------------ Boostrapping ------------------------------

(progn ;; Package configuration
    (setq package-user-dir (concat user-emacs-directory "elpa")
          package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")
                             ("cselpa" . "https://elpa.thecybershadow.net/packages/")))

    (setq use-package-always-ensure t
          use-package-expand-minimally t
          use-package-compute-statistics t
          use-package-enable-imenu-support t))

(progn ;; Boostrap `use-package`
    ;; initialize packages (only once)
    (unless (bound-and-true-p package--initialized)
        (setq package-enable-at-startup nil) (package-initialize))

    ;; install `use-package` if needed
    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

    (require 'use-package) ;; to gather statistics, I guess we need the runtime after all
    (require 'bind-key)
    (use-package diminish :defer t))

;; ------------------------------ Evil ------------------------------

(use-package evil
    :init
    ;; do not bind `C-z` to `evil-emacs-state`
    (setq evil-toggle-key "")
    :config
    (evil-mode 1))

