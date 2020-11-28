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

;; ------------------------------ Evil policies ------------------------------

(progn ;; [Evil] Use blackhole register for some keys
    ;; seems like we can't use keyboard macros for these mappings (?)

    ;; `x` -> `"_x`
    (evil-define-operator toy/null-x (beg end type register)
        :motion evil-forward-char
        (interactive "<R><x>")
        (evil-delete beg end type ?_))
    (define-key evil-normal-state-map "x" 'toy/null-x)

    ;; `s` -> `"_s` (use `d` to copy to the register)
    (evil-define-operator toy/null-s (beg end type register)
        :motion evil-forward-char
        (interactive "<R><x>")
        (evil-change beg end type ?_))
    (define-key evil-normal-state-map "s" 'toy/null-s)

    ;; more generic helper: https://github.com/syl20bnr/spacemacs/issues/6977#issuecomment-24^4014379
    )

(progn ;; [Evil] Center cursor on search (`n` -> `nzz`, `N` -> `Nzz`)
    (advice-add 'evil-ex-search-next :after (lambda (&rest x) (toy/force-center)))
    (advice-add 'evil-ex-search-previous :after (lambda (&rest x) (toy/force-center)))
    )

;; ------------------------------ Emacs-like ------------------------------

(defun toy/backward-kill-line (arg)
    (interactive "p")
    (kill-line (- 1 arg)))

;; Emacs-like in ex mode
(evil-define-key nil evil-ex-completion-map
    "\C-a" 'move-beginning-of-line
    "\C-e" 'move-end-of-line
    "\C-f" 'forward-char
    "\C-b" 'backward-char

    "\C-d" 'evil-delete-char
    "\C-h" 'evil-delete-backward-char

    "\C-k" 'evil-delete-line
    "\C-u" 'toy/backward-kill-line)

;; Emacs-like in insert mode
(evil-define-key 'insert 'global
    "\C-a" #'evil-first-non-blank
    "\C-e" #'end-of-line
    "\C-f" #'evil-forward-char
    "\C-b" #'evil-backward-char

    "\C-d" #'evil-delete-char
    "\C-h" #'evil-delete-backward-char

    ;; NOTE: it overwrites digraph key
    "\C-k" 'evil-delete-line
    "\C-u" #'toy/backward-kill-line

    ;; NOTE: prefer to use `C-n` and `C-p` for completion
    ;; "\C-n" 'evil-next-line
    ;; "\C-p" 'evil-previous-line
    )

;; ------------------------------ Misc ------------------------------

(use-package evil-nerd-commenter
    :ensure t
    :commands (evilnc-comment-or-uncomment-lines))

(evil-define-key '(normal visual) 'global
    " /" 'evilnc-comment-or-uncomment-lines)

(evil-define-key 'insert 'global
    "\C-s" (lambda () (interactive) (evil-force-normal-state) (save-buffer)))

;; ------------------------------ [] ------------------------------

(progn ;; helpers
    (defun toy/swap-line-up ()
        (let ((col (current-column)))
            (progn
                (forward-line)
                (transpose-lines -1)
                (move-to-column col)
                )))

    (defun toy/swap-line-down ()
        (interactive)
        (let ((col (current-column)))
            (progn
                (forward-line)
                (transpose-lines 1)
                (forward-line -2)
                (move-to-column col) ;; we have to manually restore the column position if we modify the line
                )))

    (defun toy/insert-line-down (count)
        (dotimes (_ count) (save-excursion (evil-insert-newline-below)))))

(evil-define-key 'normal 'global
    ;; cycle through buffers
    "[b" #'evil-prev-buffer
    "]b" #'evil-next-buffer

    ;; goto previous/next hunk and center cursor
    "[c" (lambda () (interactive)
             (git-gutter:previous-hunk 1)
             (toy/force-center))
    "]c" (lambda () (interactive)
             (git-gutter:next-hunk 1)
             (toy/force-center))

    ;; go to next/previous error and center the cursor
    "[l" (lambda () (interactive)
             (previous-error)
             (toy/force-center))
    "]l" (lambda () (interactive)
             (next-error)
             (toy/force-center))

    ;; swap lines
    "[e" (lambda () (interactive) (toy/swap-line-up))
    "]e" (lambda () (interactive) (toy/swap-line-down))

    ;; insert newline keeping the cursor position
    "[ " (lambda () (interactive)  (save-excursion (evil-insert-newline-above)))
    "] " (lambda () (interactive)  (save-excursion (evil-insert-newline-below)))
    )

