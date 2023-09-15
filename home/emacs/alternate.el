;; Initialize package.el and add Melpa repository for packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package` if it's not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load `use-package`
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(defun reload-init-file ()
  "Reload the `init.el` configuration file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

;; Configure yasnippet for code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Display snippets in a popup
  (use-package yasnippet-snippets
    :config
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
    (add-hook 'markdown-mode-hook #'yas-minor-mode)))

;; Integrate yasnippet with Ivy for snippet selection
(use-package ivy-yasnippet
  :bind (("C-c y" . ivy-yasnippet)))

;; Integrate yasnippet with Company for snippet completion
(use-package company
  :config
  (add-to-list 'company-backends 'company-yasnippet))

;; Add Evil mode for Vim-style keybindings
(use-package evil
  :init
  (setq evil-want-C-u-scroll t) ;; Enable C-u for scrolling
  :config
  (evil-mode 1)
	(evil-set-undo-system 'undo-redo))

;; Add other configuration options and packages as needed below

;; Example: Configure Org mode for a great-looking setup
(use-package org-bullets
  :config
  (setq org-ellipsis "⤵")
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1))

;; ivy integration
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

;; Example: LSP integration with lsp-mode
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (java-mode . lsp)
	 (js-mode . lsp)
	 (tsx-ts-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  ;; Configure other LSP settings as needed
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; General.el

;; Adds magit
(use-package magit
  )

;; tmux style navigation
(use-package tmux-pane
  :config
  (tmux-pane-mode)
  )

;; File tree
(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

;; Integrated term
(use-package vterm)

(use-package vterm-toggle
	:after vterm
	:config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))
	

;; Which key
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

;; All-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Status bar
(use-package doom-modeline
	:init (doom-modeline-mode 1))

;; Centaur tabs
(use-package centaur-tabs
	:demand
	:config
	(centaur-tabs-mode t)
	(setq centaur-tabs-set-icons t)
	(setq centaur-tabs-gray-out-icons 'buffer)
	(setq centaur-tabs-set-bar 'over)
	:bind
	("<tab>" . centaur-tabs-forward)
	("<backtab>" . centaur-tabs-backward))

;; Disable unrelated warnings
(setq warning-minimum-level :error)

;; Disable backup files (e.g., filename~)
(setq make-backup-files nil)

;; Disable auto-save files (e.g., #filename#)
(setq auto-save-default nil)

;; Store all backup and auto-save files in a specific directory
(setq backup-directory-alist '(("." . "~/emacs/backups/")))
(setq auto-save-file-name-transforms '((".*" "~/emacs/auto-save-list/" t)))

;; Tabs and space
(setq tab-width 2)
(setq-default ident-tabs-mode nil)

;; UI related things
(use-package dracula-theme)
(load-theme 'dracula t)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-15"))

 (add-to-list 'default-frame-alist '(alpha-background . 80))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


;; Keybinds 
(use-package general
    :config
    (general-evil-setup)
    (general-create-definer ys/leader-keys
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC"
      :global-prefix "M-SPC")


    (ys/leader-keys
      "f" '(:ignore t :wk "projectile")
      "ff" '(projectile-find-file :wk "Find file")
      "fb" '(projectile-switch-to-buffer :wk "Switch to buffer")
      )

		(ys/leader-keys
			"s" '(:ignore t :wk "window")
			"sh" '(evil-window-split :wk "Horizontal split")
      "sv" '(evil-window-vsplit :wk "Vertical split"))


   (ys/leader-keys
"t" '(vterm-toggle :wk "vterm")
)

    (ys/leader-keys
      "o" '(:ignore t :wk "Org")
      "oa" '(org-agenda :wk "Org agenda")
      "oe" '(org-export-dispatch :wk "Org export")
      "oi" '(org-toggle-item :wk "Org toggle Item")
      "ot" '(org-todo :wk "Org Todo")
      "oT" '(org-todo-list :wk "Org Todo List")
      )

    (ys/leader-keys
      "g" '(magit :wk "Open magit"))
		)

;; End of init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f25f174e4e3dbccfcb468b8123454b3c61ba94a7ae0a870905141b050ad94b8f" default))
 '(package-selected-packages
   '(centaur-tabs doom-modeline org-bullets general which-key neotree company-lsp org-plus-contrib evil yasnippet-snippets vterm-toggle tree-sitter-langs magit all-the-icons lsp-ui lsp-treemacs ivy-yasnippet company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
