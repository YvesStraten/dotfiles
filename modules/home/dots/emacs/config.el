(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :files (:defaults (:exclude "extensions"))
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
    (setq
evil-want-keybinding nil
evil-vsplit-window-right t
evil-split-window-below t)
   (evil-mode) )

(use-package evil-collection
:after evil
:config
(setq evil-collection-mode-list '(dashborad dired ibuffer))
(evil-collection-init))

(use-package evil-tutor)

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
                "bp" '(previous-buffer :wk "Previous buffer")
    "p" '(:ignore t :wk "projectile")
         "pp" '(helm-projectile :wk "Go to project") 
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
"w" '(:ignore t :wk "Windows")
   ;; Window splits
  "w c" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  ;; Window motions
  "w h" '(evil-window-left :wk "Window left")
  "w j" '(evil-window-down :wk "Window down")
  "w k" '(evil-window-up :wk "Window up")
  "w l" '(evil-window-right :wk "Window right")
  "w w" '(evil-window-next :wk "Goto next window")
  ;; Move Windows
  "w H" '(buf-move-left :wk "Buffer move left")
  "w J" '(buf-move-down :wk "Buffer move down")
  "w K" '(buf-move-up :wk "Buffer move up")
  "w L" '(buf-move-right :wk "Buffer move right"))
)

(require 'windmove)

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(set-face-attribute 'default nil
  :font "JetBrainsMono NF"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono NF"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(setq
        case-fold-search nil
          use-short-answers t
          confirm-kill-processes nil
      make-backup-files nil
  auto-save-default nil
create-lockfiles nil)

(use-package toc-org
  :commands toc-org-enable
:init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)

(electric-pair-mode 1)

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

(use-package diminish)

(use-package all-the-icons
:ensure t
:if (display-graphic-p))

(use-package rainbow-mode
 :diminish
 :hook org-mode prog-mode)

(use-package lsp-mode
    :ensure t
          :init
          (setq lsp-keymap-prefix "C-c l"
          lsp-log-io nil
          lsp-restart 'auto-restart
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-code-actions t
          lsp-eslint-auto-fix-on-save t
)
          :hook (
    (prog-mode-hook . lsp)
    (lsp-mode . lsp-enable-which-key-integration))
    :commands (lsp lsp-deferred)
          )

(use-package typescript-mode
:mode "\\.ts\\'"
:hook (typescript-mode . lsp-deferred)
:config
(setq typescript-ident-level 2))

(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode)

  :custom (
lsp-ui-doc-position 'bottom))
      (use-package helm-lsp :commands helm-lsp-workspace-symbol)
      (use-package helm-projectile :commands helm-projectile)
      (use-package dap-mode)

(use-package tree-sitter
:hook (typescript-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(make-variable-buffer-local 'show-paren-mode)
(show-paren-mode 1)
   (setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

  (use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))



(use-package projectile
:config
(projectile-mode 1))

(use-package company 
:after lsp-mode
       :diminish
:custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (global-company-mode 1))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))
  (defun check-expansion ()
     (save-excursion
       (if (looking-at "\\_>") t
         (backward-char 1)
         (if (looking-at "\\.") t
           (backward-char 1)
           (if (looking-at "->") t nil)))))

   (defun do-yas-expand ()
     (let ((yas/fallback-behavior 'return-nil))
       (yas/expand)))

   (defun tab-indent-or-complete ()
     (interactive)
     (if (minibufferp)
         (minibuffer-complete)
       (if (or (not yas/minor-mode)
               (null (do-yas-expand)))
           (if (check-expansion)
               (company-complete-common)
             (indent-for-tab-command)))))

   (global-set-key [tab] 'tab-indent-or-complete)

(use-package yasnippet
:diminish yas-minor-mode
:config
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(yas/global-mode)
)

(use-package yasnippet-snippets)

(use-package magit
:ensure t)

(use-package neotree
        :ensure t
      :config
(ys/leader-keys
"n" '(neotree-toggle :wk "Toggle neotree"))
  (setq
neo-theme 'icons
neo-smart-open t
neo-show-hidden-file t
neo-window-width 30
projectile-switch-project-action 'neotree-projectile-action)
(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(add-hook 'neo-after-create-hook
#'(lambda (_)
   (with-current-buffer (get-buffer neo-buffer-name)
       (setq truncate-lines t)
       (setq word-wrap nil)
       (make-local-variable 'auto-hscroll-mode)
       (setq auto-hscroll-mode nil)))))

(use-package vterm
   (ys/leader-keys
"t" '(vterm-toggle :wk "term")
))

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
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package sudo-edit
 :config
  (ys/leader-keys
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fU" '(sudo-edit :wk "Sudo edit file")))

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

(defun reload-init-file ()
 (interactive)
 (load-file user-init-file)
 (load-file user-init-file)
)
