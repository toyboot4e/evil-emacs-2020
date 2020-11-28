;; ------------------------------ Setup GC ------------------------------
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org

;; https://emacs-lsp.github.io/lsp-mode/page/performance
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq toy/gc 100000000) ; 100MB

;; maxmize GC on startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; and then reset it to the  preferred value (`toy/gc`)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold toy/gc
                           gc-cons-percentage 0.1)))

;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 60.0 t #'garbage-collect)

;; Prevent GC from happing in minibuffer
(defun toy/gc-minibuf () (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'toy/gc-minibuf)

;; Restore the preferred value after 1 second
(defun toy/gc-restore () (run-at-time 1 nil (lambda () (setq gc-cons-threshold toy/gc))))
(add-hook 'minibuffer-exit-hook #'toy/gc-restore)

