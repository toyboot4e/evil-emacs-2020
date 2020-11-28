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

