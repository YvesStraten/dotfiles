(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
    (elpaca elpaca-use-package
      ;; Enable :elpaca use-package keyword.
      (elpaca-use-package-mode)
      ;; Assume :elpaca t unless otherwise specified.
      (setq elpaca-use-package-by-default t))

    ;; Block until current queue processed.
    (elpaca-wait)

    ;;When installing a package which modifies a form used at the top-level
    ;;(e.g. a package which adds a use-package key word),
    ;;use `elpaca-wait' to block until that package has been installed/configured.
    ;;For example:
    ;;(use-package general :demand t)
    ;;(elpaca-wait)

    ;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init
  (evil-mode))

    ;;Turns off elpaca-use-package-mode current declartion
    ;;Note this will cause the declaration to be interpreted immediately (not deferred).
    ;;Useful for configuring built-in emacs features.
    (use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

    ;; Don't install anything. Defer execution of BODY
    (elpaca nil (message "deferred"))

(use-package general
      :config
(general-evil-setup)
  (general-create-definer ys/leader-keys
	  :states '(normal insert visual emacs)
	  :keymaps 'override
	  :prefix "SPC"
	  :global-prefix "M-SPC")


	  (ys/leader-keys
	"b" '(:ignore t :wk "buffer")
	    "bb" '(switch-to-buffer :wk "Switch buffer")
	    "bk" '(kill-this-buffer :wk "Kill this buffer")
	    "bn" '(next-buffer :wk "Next buffer")
	    "bp" '(previous-buffer :wk "Previous buffer"))


	  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(use-package toc-org
  :commands toc-org-enable
:init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit t))

(use-package all-the-icons
:ensure t
:if (display-graphic-p))

(use-package lsp-mode
:ensure t)

(use-package company
    :ensure t
  :config
(setq company-idle-delay 0.1
      company-minimum-prefix-length 1))

(use-package yasnippet
:config
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(setq yas-global-mode 1)
)

(use-package neotree
:ensure t)

(use-package vterm
:ensure t)

(use-package dashboard
:ensure t
:init
(setq initial-buffer-choice 'dashboard-open)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

:config
(dashboard-setup-startup-hook))

(use-package timu-spacegrey-theme
      :ensure t
    :config
  (load-theme 'timu-spacegrey t)
(customize-set-variable 'timu-spacegrey-flabour "dark"))