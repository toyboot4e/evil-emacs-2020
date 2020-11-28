;; ------------------------------ Widgets ------------------------------

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

