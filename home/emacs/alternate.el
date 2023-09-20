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

;; Display snippets in a popup
(use-package yasnippet-snippets)

;; Configure yasnippet for code snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;; Integrate yasnippet with Ivy for snippet selection
(use-package ivy-yasnippet
  :bind (("C-c y" . ivy-yasnippet)))

(use-package lsp-ivy
  :bind (("C-C l" . lsp-ivy-workspace-symbol)))

;; Integrate yasnippet with Company for snippet completion
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1
	company-dabbrev-downcase 0
	company-box-doc-enable nil))

;; Company box
(use-package company-box
  :hook (company-mode . company-box-mode))

(setq company-box-icons-unknown 'fa_question_circle)
(setq company-box-icons-elisp
      '((fa_tag :face font-lock-function-name-face) ;; Function
	(fa_cog :face font-lock-variable-name-face) ;; Variable
	(fa_cube :face font-lock-constant-face) ;; Feature
	(md_color_lens :face font-lock-doc-face))) ;; Face
(setq company-box-icons-yasnippet 'fa_bookmark)
(setq company-box-icons-lsp
      '((1 . fa_text_height) ;; Text
        (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
        (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
        (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
        (5 . (fa_cog :foreground "#FF9800")) ;; Field
        (6 . (fa_cog :foreground "#FF9800")) ;; Variable
        (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
        (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
        (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
        (10 . (fa_cog :foreground "#FF9800")) ;; Property
        (11 . md_settings_system_daydream) ;; Unit
        (12 . (fa_cog :foreground "#FF9800")) ;; Value
        (13 . (md_storage :face font-lock-type-face)) ;; Enum
        (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
        (15 . md_closed_caption) ;; Snippet
        (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
        (17 . fa_file_text_o) ;; File
        (18 . md_refresh) ;; Reference
        (19 . fa_folder_open) ;; Folder
        (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
        (21 . (fa_square :face font-lock-constant-face)) ;; Constant
        (22 . (fa_cube :face font-lock-type-face)) ;; Struct
        (23 . fa_calendar) ;; Event
        (24 . fa_square_o) ;; Operator
        (25 . fa_arrows)) ;; TypeParameter
      )


;; Add Evil mode for Vim-style keybindings
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-keybinding nil) ;; Enable C-u for scrolling
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

;; Evil collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-bullets
  :config
  (setq org-ellipsis "⤵")
  (add-hook 'org-mode-hook 'org-bullets-mode ))

					; (use-package org-fragtog
					;   :config
					;   (add-hook 'org-mode-hook 'org-fragtog-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (C . t)
   (java . t)
   (python . t)))

;; Credit to Derek Taylor for these agenda commands
(setq
 ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
 ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
 org-fancy-priorities-list '("🟥" "🟧" "🟨")
 org-priority-faces
 '((?A :foreground "#ff6c6b" :weight bold)
   (?B :foreground "#98be65" :weight bold)
   (?C :foreground "#c678dd" :weight bold))
 org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

;; Latex
					; (use-package xenops
					;   :config
					;   (add-hook 'latex-mode-hook #'xenops-mode)
					;   (add-hook 'LaTeX-mode-hook #'xenops-mode)
					;   (add-hook 'org-mode-hook #'xenops-mode)
					;   )

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1))

;; ivy integration
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(require 'tree-sitter-langs)
(require 'tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Example: LSP integration with lsp-mode
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (java-mode . lsp)
	 (typescript-mode . lsp)
	 (js-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
	lsp-prefer-capf t
	gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.500
	lsp-log-io nil)
  ;; Configure other LSP settings as needed
  )

;; For java
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

;; For debugging
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; For the vscode themed minimap
(use-package minimap
  )

;; Formatter
(use-package format-all
  :config
  (add-hook 'prog-mode-hook 'format-all-mode))

;; Indent blankline
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'top)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package lsp-treemacs
  :after lsp)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Markdown mode
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

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
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
	neo-window-width 25
	neo-smart-open t
	neo-show-hidden-files t)
  :bind
  (:map evil-normal-state-map
        ("C-n" . neotree-toggle))
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
  (centaur-tabs-headline-match)
  (setq centaur-tabs-height 40
	centaur-tabs-style "wave"
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t
	centaur-tabs-set-modified-marker t)
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward))
  )

;; Language tool
;; Credits to doom emacs developers
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

;; Automatically reverts buffers for changed files
(global-auto-revert-mode 1)

;; Reverts dired as well
(setq global-auto-revert-non-file-buffers t)

;; Remembers the last place you visited in a file
(save-place-mode 1)

;; Wipes scrollbars
(scroll-bar-mode -1)

;; Disable unrelated warnings
(setq warning-minimum-level :error)

;; Disable backup files (e.g., filename~)
(setq make-backup-files nil)

;; Disable auto-save files (e.g., #filename#)
(setq auto-save-default nil)

;; Disable lock file creation
(setq create-lockfiles nil)

;; Store all backup and auto-save files in a specific directory
(setq backup-directory-alist '(("." . "~/emacs/backups/")))
(setq auto-save-file-name-transforms '((".*" "~/emacs/auto-save-list/" t)))

;; Removes annoying prompts
(setq use-short-answers t)

;; Tabs and space
(setq tab-width 2)
(setq-default ident-tabs-mode nil)

(use-package ligature
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

;; UI related things
;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome to Emacs")
(setq dashboard-startup-banner 'official)
(setq dashboard-center-content t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-heading-icons t)

;; Sets which dashboard items should show
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

;; Uses dracula theme
(use-package dracula-theme)
(load-theme 'dracula t)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-15"))
(setq display-line-numbers 'relative
      display-line-numbers-current-absolute t)

;; For never losing the cursor
(use-package beacon
  :config
  (setq beacon-blink-when-window-scrolls t)
  (beacon-mode 1))

;; For jumping in the file
(use-package avy)

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
    "fp" '(projectile-switch-project :wk "Switch project")
    )

  (ys/leader-keys
    "x" '(kill-this-buffer :wk "Kill buffer"))

  (ys/leader-keys
    "j" '(avy-goto-char-2 :wk "Search buffer"))

  (ys/leader-keys
    "s" '(:ignore t :wk "window")
    "sh" '(evil-window-split :wk "Horizontal split")
    "sv" '(evil-window-vsplit :wk "Vertical split"))


  (ys/leader-keys
    "t" '(vterm-toggle :wk "vterm")
    )

  (ys/leader-keys
    "l" '(:ignore t :wk "Lsp")
    "lr" '(lsp-rename :wk "Rename reference")
    "lf" '(format-all-buffer
	   :wk "Formats buffer"))

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
 '(package-selected-packages
   '(ligature beacon vterm-toggle tree-sitter-langs magit lsp-java company-box all-the-icons yasnippet-snippets which-key typescript-mode toc-org tmux-pane smartparens rainbow-mode rainbow-delimiters org-bullets nix-mode neotree minimap lsp-ui lsp-ivy langtool ivy-yasnippet highlight-indent-guides general format-all evil-nerd-commenter evil-collection dracula-theme doom-modeline dashboard counsel-projectile centaur-tabs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
