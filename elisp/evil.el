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

;; show line numbers
(global-display-line-numbers-mode)

;; highlight current line
(global-hl-line-mode t)

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

;; ------------------------------ View settings ------------------------------

;; Color scheme in use
(use-package darkokai-theme
    :config (load-theme 'darkokai t))

;; ------------------------------ Evil ------------------------------

(use-package evil
    :init
    ;; do not bind `C-z` to `evil-emacs-state`
    (setq evil-toggle-key "")
    :config
    (evil-mode 1))

(progn ;; Interactive commands
    ;; use `:ed` to open `init.el`
    (evil-ex-define-cmd "ed" (lambda () (interactive) (evil-edit (concat user-emacs-directory "init.el"))))
    ;; use `:s` to edit `init.el`
    (evil-ex-define-cmd "s" (lambda () (interactive) (load-file (concat user-emacs-directory "init.el"))))
    ;; close buffer (without closing window). alternative: `C-x k RET`
    (evil-ex-define-cmd "Bd" #'kill-this-buffer)
    (evil-ex-define-cmd "BD" #'kill-this-buffer)
    ;; horizontal split
    (evil-ex-define-cmd "hs" #'evil-split-buffer))

;; Enable redo with evil
(use-package undo-tree
    :init
    (evil-set-undo-system 'undo-tree)
    (global-undo-tree-mode))

;; ------------------------------ Programming ------------------------------

(progn ;; ELisp
    (setq-default lisp-body-indent 4    ; I need this
                  indent-tabs-mode nil  ; don't whitespaces with TAB (overwrite it for, e.g., Makefile)
                  tab-width 4           ; display tab with a width of 4
                  )


    (use-package aggressive-indent
        :hook ((emacs-lisp-mode scheme-mode) . aggressive-indent-mode))

    ;; enable folding (`zr` to open all, `zm` to fold all, `za` to toggle)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    )

