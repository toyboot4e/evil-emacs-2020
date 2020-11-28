;; early-init.el

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
    (push '(ns-transparent-titlebar . t) default-frame-alist))

