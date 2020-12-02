;; ------------------------------ Hydra ------------------------------

;; builtin!
(require 'winner)
(winner-mode 1)

(use-package windswap
    ;; https://github.com/amnn/windswap
    ;; windswap-left|right|up|down
    :commands (windswap-up windswap-down windswap-left windswap-right))

(defun toy/sp-W ()
    "Split west."
    (interactive)
    (evil-window-vsplit)
    ;; without focusing to it
    (evil-window-right 1))

(defun toy/sp-N ()
    "Split north."
    (interactive)
    (evil-window-split)
    ;; without focusing to it
    (evil-window-down 1))

(defvar toy/expand-unit 5)

(defhydra toy/hydra-window (:color red :hint nil)
    "
hjkl: focus     rR: rotate         u: undo window change
HJKL: resize    =: equlize
wasd: split     c/q: close
WASD: swap      x: kill, X: both
"

    ("u" #'winner-undo)
    ;; doesn't work
    ;; ("C-r" winner-redo)

    ;; focus
    ("h" #'evil-window-left)
    ("j" #'evil-window-down)
    ("k" #'evil-window-up)
    ("l" #'evil-window-right)

    ;; enlarge/shrink
    ("H" (lambda () (interactive) (enlarge-window-horizontally (- toy/expand-unit))))
    ("L" (lambda () (interactive) (enlarge-window-horizontally toy/expand-unit)))
    ("K" (lambda () (interactive) (enlarge-window (- toy/expand-unit))))
    ("J" (lambda () (interactive) (enlarge-window toy/expand-unit)))

    ;; split
    ("w" #'toy/sp-N)
    ("a" #'toy/sp-W)
    ("s" #'evil-window-split)
    ("d" #'evil-window-vsplit)

    ("W" (lambda () (interactive) (windswap-up)))
    ("A" (lambda () (interactive) (windswap-left)))
    ("S" (lambda () (interactive) (windswap-down)))
    ("D" (lambda () (interactive) (windswap-right)))

    ;; close
    ("c" #'evil-quit)
    ("q" #'evil-quit)

    ;; delete
    ("x" #'kill-this-buffer)
    ("X" #'evil-delete)

    ("C-s" #'save-buffer)

    ("r" #'evil-window-rotate-downwards)
    ("R" #'evil-window-rotate-upwards)
    ("=" #'balance-windows)

    ("z" #'toy/zen)
    ("b" 'evil-buffer-new)

    ("ESC" nil)
    )

