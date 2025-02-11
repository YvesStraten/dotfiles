(setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  "Defer gc."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore gc."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable startup message
(defun display-startup-echo-area-message ()
  (message nil))

;; Disable before actual init
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(push '(width . 0.5) default-frame-alist)
(push '(height . 0.5) default-frame-alist)
(setq site-run-file nil)
