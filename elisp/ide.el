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

