(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install use-package support
;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init
  (setq
   evil-want-keybinding nil
   evil-vsplit-window-right t
   evil-split-window-below t
   evil-set-undo-system 'undo-redo
   )
  (evil-mode) )

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)

(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

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
      "bn" '(next-buffer :wk "Next buffer")
      "bp" '(previous-buffer :wk "Previous buffer")
      "p" '(:ignore t :wk "projectile")
      "pp" '(helm-projectile :wk "Go to project") 
      "pf" '(helm-projectile-find-file :wk "Find file")
      "pb" '(helm-projectile-switch-to-buffer :wk "Switch to buffer")
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
      "c" '(:ignore t :wk "Tabs")
      "cn" '(centaur-tabs-forward :wk "Next tab")
      "cp" '(centaur-tabs-backward :wk "Previous tab")
      "cx" '(kill-this-buffer :wk "Kill this buffer")
      )

    (ys/leader-keys
      "g" '(magit :wk "Open magit"))

    (ys/leader-keys
"c" '(evilnc-comment-or-uncomment-lines :wk "Toggle comment"))

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

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file)
  )

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

(use-package centaur-tabs
  :defer 5
  :diminish centaur-tabs-mode
  :config (centaur-tabs-mode t)
  )

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono NF"))

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

(use-package linum-relative
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

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

(custom-set-variables
 '(org-directory "~/org")
 '(org-agenda-files (list org-directory)))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(org-babel-do-load-languages
'org-babel-load-languages
'((js . t)
(python . t)))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook (lambda ()
         (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

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
  :if (display-graphic-p))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook org-mode prog-mode)

(use-package evil-nerd-commenter
)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-restart 'auto-restart
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
  (setq-default typescript-indent-level 2)
  )

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package lsp-ui :hook (lsp-mode . lsp-ui-mode)

  :custom (
           lsp-ui-doc-position 'bottom))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package helm-projectile :commands helm-projectile)
(use-package dap-mode)

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  :config
  (setq highlight-indent-guides-responsive 'top))

(use-package lsp-treemacs
:after lsp)

(use-package lsp-ivy)

(use-package format-all
  :init
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  )

(use-package tree-sitter
  :hook (typescript-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(make-variable-buffer-local 'show-paren-mode)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package company 
  :init (global-company-mode)
  :diminish company-mode
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  )

(defvar company-backends nil)
(add-to-list 'company-backends '(company-yasnippet company-dabbrev))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas/global-mode)
  )
(setq yas-snippet-dirs '("~/Git-repos/dotfiles/modules/home/dots/snippets"))
(use-package yasnippet-snippets)

(use-package magit
  :diminish magit-mode
  )

(use-package neotree
  :straight t
  :config
  (ys/leader-keys
    "n" '(neotree-toggle :wk "Toggle neotree"))
  (setq
   neo-theme 'icons
   neo-smart-open t
   neo-show-hidden-file t
   neo-window-width 30)
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
               (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
               (define-key evil-normal-state-local-map (kbd "a") 'neotree-create-node)
               (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)
               (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
   (add-hook 'neo-after-create-hook
             #'(lambda (_)
                 (with-current-buffer (get-buffer neo-buffer-name)
                   (setq truncate-lines t)
                   (setq word-wrap nil)
                   (make-local-variable 'auto-hscroll-mode)
                   (setq auto-hscroll-mode nil)))))

(use-package vterm
   :config
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
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package sudo-edit
  :config
  (ys/leader-keys
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fU" '(sudo-edit :wk "Sudo edit file")))

(use-package dashboard
:straight t
:init
(setq initial-buffer-choice 'dashboard-open)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

:config
(dashboard-setup-startup-hook))

(use-package catppuccin-theme 
  :straight t
  :config
  (load-theme 'catppuccin t)
  (setq catppuccin-flavor 'mocha)
  )

(use-package langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond ((setq langtool-bin
                 (or (executable-find "languagetool-commandline")
                     (executable-find "languagetool")))))))  ; for nixpkgs.languagetool
