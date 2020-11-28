;; ------------------------------ More to Evil ------------------------------

;; Be Evil everywhere: https://github.com/noctuid/evil-guide#use-evil-everywhere
(setq evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil)

(define-key help-mode-map "q" #'kill-this-buffer)
(define-key help-mode-map "Q" #'evil-delete-buffer)

;; [Evil] Smart escape with `jk` or `kj`
(use-package evil-escape
    :init (setq evil-escape-key-sequence "jk"
                evil-escape-unordered-key-sequence t)
    :config (evil-escape-mode))


