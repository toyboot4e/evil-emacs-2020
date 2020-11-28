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

(progn ;; [Evil] Set up word policy
    ;; Do not treat `_` as a word boundary (thought it still treats `-` as a word boundary):
    (modify-syntax-entry ?_ "w")
    ;; Or, make motions based on _symbols_, instead of _words_:
    ;; (defalias 'forward-evil-word 'forward-evil-symbol)

    ;; c.f. https://evil.readthedocs.io/en/latest/faq.html
    )

(progn ;; [Evil] Prevent cursor from going to the next line of EoF
    (defun toy/fix-point ()
        (unless (window-minibuffer-p)
            (when (= (point) (point-max)) (forward-line -1))))
    (add-hook 'post-command-hook (lambda () (interactive) (toy/fix-point))))

;; [Evil] Let `{` and `}` skip multiple bullets (`* ..` in markdown) like Vim:
(with-eval-after-load 'evil
    (defadvice forward-evil-paragraph (around default-values activate)
        (let ((paragraph-start (default-value 'paragraph-start))
              (paragraph-separate (default-value 'paragraph-separate)))
            ad-do-it)))

