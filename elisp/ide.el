;; Put auto-generated files in `tmp` directory
(setq projectile-cache-file (concat user-emacs-directory "tmp/projectile.cache")
      projectile-known-projects-file (concat user-emacs-directory "tmp/projectile-bookmarks.eld")
      ;; magit
      transient-history-file (concat user-emacs-directory "tmp/transient/history.el")
      transient-values-file (concat user-emacs-directory "tmp/transient/values.el")
      transient-levels-file (concat user-emacs-directory "tmp/transient/levels.el")
      )

;; ------------------------------ Widgets ------------------------------

;; Zoom in to a pane: https://github.com/emacsorphanage/zoom-window
(use-package zoom-window)

;; Zen mode *per buffer* (not per frame and that is great!)
(use-package olivetti
    ;; https://github.com/rnkn/olivetti
    :commands (olivetti-mode)
    :custom
    (olivetti-body-width 100))

(use-package magit
    ;; https://github.com/magit/magit
    :commands (magit)
    ;; NOTE: use Zen mode in `magit-status`
    :hook (magit-status-mode  . olivetti-mode)
    :init (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
    ;; https://github.com/emacs-evil/evil-magit
    :after (evil magit)
    :custom
    ;; add fold mappings (`z1`, `z2`, .., `za`, ..)
    (evil-magit-use-z-for-folds t)
    :config
    ;; enable scolling key mappings
    (evil-define-key 'normal magit-mode-map
        "zz" #'evil-scroll-line-to-center
        "z-" #'evil-scroll-line-to-bottom
        (kbd "z RET") #'evil-scroll-line-to-top
        ))

(use-package centaur-tabs
    ;; https://github.com/ema2159/centaur-tabs
    ;; NOTE: it adds default bindings go to `C-c C-c`
    :config
    ;; show buffers in current group
    (setq centaur-tabs--buffer-show-groups nil)

    ;; navigate through buffers only in the current group
    (setq centaur-tabs-cycle-scope 'tabs)

    ;; show underline for the current buffer
    (setq centaur-tabs-set-bar 'under
          x-underline-at-descent-line t)

    ;; configure
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 24
          centaur-tabs-set-modified-marker t
          centaur-tabs-gray-out-icons 'buffer ;; gray out unfocused tabs
          centaur-tabs-show-navigation-buttons nil
          centaur-tabs-set-icons (display-graphic-p)
          )
    ;; (centaur-tabs-group-by-projectile-project)

    (centaur-tabs-headline-match)
    (centaur-tabs-mode t))

(use-package neotree
    :after evil
    :commands (neotree-quick-look)
    :init
    (setq neo-theme (if (display-graphic-p) 'icons 'arrows)
          neo-window-position 'right  ;; show on right side
          neo-window-width 25         ;; preferred with
          neo-window-fixed-size nil   ;; resize with mouse
          neo-show-hidden-files t     ;; show hidden files
          )

    :config
    ;; [GUI] set fontsize
    (when (display-graphic-p)
        (add-hook 'neo-after-create-hook
                  (lambda (_)
                      (text-scale-adjust 0)
                      (text-scale-decrease 0.5) ;; the bigger, the smaller
                      )))

    (evil-define-key 'normal neotree-mode-map
        (kbd "RET") #'neotree-enter

        ;; open
        "oo" #'neotree-enter
        "ov" #'neotree-enter-vertical-split
        "oh" #'neotree-enter-horizontal-split

        ;; change directory
        "cd" #'neotree-change-root    ;; cd: chande directory
        "cu" #'neotree-select-up-node ;; cu: up directory
        "cc" #'neotree-copy-node      ;; cc: copy

        ;; menu (file/directory operation)
        "mc" #'neotree-create-node    ;; mc: create node
        "md" #'neotree-delete-node    ;; md: delete node
        "mr" #'neotree-rename-node    ;; mr: rename mode

        ;; view
        "h" #'neotree-hidden-file-toggle
        "r" #'neotree-refresh

        ;; else
        "q" #'neotree-hide          ;; quit
        "z" #'neotree-stretch-toggle
        (kbd "TAB") 'neotree-stretch-toggle
        )
    )

;; ------------------------------ Intelligence ------------------------------

(use-package company ;; COMPlete ANYthing
    ;; http://company-mode.github.io/
    :hook (prog-mode . company-mode)
    :bind (:map company-active-map
                ;; ("<tab>" . company-complete-selection)
                ("C-n" . company-select-next-or-abort)
                ("C-p" . company-select-previous-or-abort))
    :init
    (setq company-idle-delay 0             ;; default: `0.2`
          company-minimum-prefix-length 1
          company-selection-wrap-around t
          ))

(use-package company-box
    ;; show icons in the completion menu
    ;; https://github.com/sebastiencs/company-box
    :if (display-graphic-p)
    :hook (company-mode . company-box-mode))

(use-package ivy
    ;; https://github.com/abo-abo/swiper
    :init
    (setq ivy-use-virtual-buffers nil ; show recentf and bookmarks?
          ivy-count-format "(%d/%d) "
          ivy-height 20
          ivy-truncate-lines t        ; trancate or wrap
          ivy-wrap t                  ; cycle, please!
          )
    :bind (:map ivy-minibuffer-map
                ;; Vim-like
                ("C-f" . ivy-scroll-up-command)
                ("C-b" . ivy-scroll-down-command)
                ;; press `C-l` to preview (default: `C-M-m`)
                ("C-l" . ivy-call)
                ;; press `C-k k` to kill buffer (without closing Ivy)
                ("C-k" . ivy-switch-buffer-kill)
                ;; press `C-,` to open menu
                ;; NOTE: never map it to `C-m`; it's carriage-return in terminal
                ("C-," . ivy-dispatching-call)
                )
    :config (ivy-mode))

(use-package ivy-rich
    ;; https://github.com/Yevgnen/ivy-rich
    :after ivy
    :init (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    :config (ivy-rich-mode 1))

(use-package projectile
    ;; https://github.com/bbatsov/projectile
    :init
    (setq projectile-completion-system 'ivy
          projectile-enable-caching t)
    :config (projectile-mode +1))

(use-package counsel
    :after ivy
    :bind
    ;; override default functions (`C-h f` etc.)
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-bindings] . counsel-descbinds)
    ;; see also: `C-h M` (which is mapped to `which-key-show-major-mode`)
    )

(use-package counsel-projectile
    :after (evil counsel projectile)
    :config
    ;; overwrite key mappings for `i_CTRL-r` (with Ivy height 20)
    (evil-define-key 'insert 'global "\C-r" #'counsel-evil-registers)
    ;; NOTE: we can't use `counsel-evil-registers` in ex mode because both of them use minibuffer
    (add-to-list 'ivy-height-alist '(counsel-evil-registers . 20)))

(use-package swiper
    :after evil
    :config (evil-define-key 'normal 'global
                "*" 'swiper-thing-at-point
                ))

(use-package hydra
    ;; https://github.com/abo-abo/hydra
    :defer t)

;; ------------------------------ DSLs ------------------------------

(use-package gitignore-mode
    :mode ("\\.gitignore" . gitignore-mode))

(use-package markdown-mode
    ;; https://jblevins.org/projects/markdown-mode/
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")
    :config
    (evil-define-key 'normal markdown-mode-map
        "z1" (lambda () (interactive) (outline-hide-sublevels 1))
        "z2" (lambda () (interactive) (outline-hide-sublevels 2))
        "z3" (lambda () (interactive) (outline-hide-sublevels 3))
        "z4" (lambda () (interactive) (outline-hide-sublevels 4))
        "z5" (lambda () (interactive) (outline-hide-sublevels 5))
        "z6" (lambda () (interactive) (outline-hide-sublevels 6))
        "z9" (lambda () (interactive) (outline-hide-sublevels 9))
        "z0" #'evil-open-folds))

(use-package adoc-mode
    ;; https://github.com/sensorflo/adoc-mode
    ;; c.f. `evil-lion` to align table
    :mode (("\\.adoc\\'" . adoc-mode))
    :hook (adoc-mode . outline-minor-mode)
    :config
    (evil-define-key 'normal outline-minor-mode-map
        "z1" (lambda () (interactive) (outline-hide-sublevels 3))
        "z2" (lambda () (interactive) (outline-hide-sublevels 4))
        "z3" (lambda () (interactive) (outline-hide-sublevels 5))
        "z4" (lambda () (interactive) (outline-hide-sublevels 6))
        "z5" (lambda () (interactive) (outline-hide-sublevels 7))
        "z6" (lambda () (interactive) (outline-hide-sublevels 8))
        "z9" (lambda () (interactive) (outline-hide-sublevels 11))
        "z0" #'evil-open-folds
        ))

(use-package yaml-mode :defer t)
(use-package ron-mode
    :mode (("\\.ron\\'" . ron-mode))
    ;; for `evil-nerd-commenter`:
    :hook (ron-mode . (lambda () (setq comment-start "// " comment-end "")))
    )

