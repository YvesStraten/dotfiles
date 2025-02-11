(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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

(elpaca elpaca-use-package 
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)
;;  (setq use-package-always-defer t)

(defun reload-init-file ()
  "Reload the `init.el` configuration file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(use-package bug-hunter)

(use-package esup)

(use-package exec-path-from-shell
  :demand t
  :ensure (:wait t)
  :config
  (dolist (var '("PYTHONPATH"         ; Python modules
                 "INFOPATH"           ; Info directories
                 "JAVA_OPTS"          ; Options for java processes
                 "SBT_OPTS"           ; Options for SBT
                 "RUST_SRC_PATH"      ; Rust sources, for racer
                 "CARGO_HOME"         ; Cargo home, for racer
                 "EMAIL"              ; My personal email
                 "GPG_TTY"
                 "GPG_AGENT_INFO"
                 "SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "JDTLS_PATH"
                 "HOME"
                 ))

    (add-to-list 'exec-path-from-shell-variables var)
    ))

  (exec-path-from-shell-initialize)

(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)
;; Fixes switch statements
(c-set-offset 'case-label '+)

(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        scroll-margin 8
        ) ;; Enable C-u for scrolling
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :demand
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil)

(use-package simpleclip
  :init (simpleclip-mode 1))

(use-package general
:ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer ys/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (ys/leader-keys
    "x" '(kill-this-buffer :wk "Kill buffer"))

  (ys/leader-keys
    "j" '(avy-goto-char-2 :wk "Search buffer"))

  (ys/leader-keys
    "s" '(:ignore t :wk "window")
    "sh" '(evil-window-split :wk "Horizontal split")
    "sv" '(evil-window-vsplit :wk "Vertical split")
    "sp" '(langtool-check :wk "Check with langtool")
    "sk" '(flyspell-correct-wrapper :wk "Flyspell correct")
    "sc" '(:ignore t :wk "Correct")
    "scp" '(langtool-correct-at-point :wk "Correct at point")
    "scb" '(langtool-correct-buffer :wk "Correct buffer"))

  (ys/leader-keys
    "p" '(:ignore t :wk "Project"))

  (ys/leader-keys
    "c" '(:ignore t :wk "Code")
    "cc" '(compile :wk "Compile")
    "cC" '(recompile :wk "Recompile"))

  (ys/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "bx" '(kill-buffer :wk "Kill Buffer")
    "bi" '(ibuffer :wk "Ibuffer"))

  (ys/leader-keys
    "l" '(:ignore t :wk "Lsp"))

  (ys/leader-keys
    "o" '(:ignore t :wk "Org")
    "ob" '(org-mark-ring-goto :wk "Travel to origin link")
    "oa" '(org-agenda :wk "Org agenda")
    "oe" '(org-export-dispatch :wk "Org export")
    "oi" '(org-toggle-item :wk "Org toggle Item")
    "ot" '(org-todo :wk "Org Todo")
    "oT" '(org-todo-list :wk "Org Todo List")
    "op" '(org-tree-slide-mode :wk "Present"))

  (ys/leader-keys
    "y" '(simpleclip-copy :wk "Yank to clipboard"))

  (ys/leader-keys
    "P" '(simpleclip-paste :wk "Paste from clipboard"))
  )

(defun my/persp-switch (name)
  (interactive "s")
  (persp-switch name)
  (switch-to-buffer "*dashboard*")
  )

(use-package perspective
  :hook (dashboard-mode . persp-mode)
  :general (ys/leader-keys
                       "TAB" '(:ignore t :wk "Workspaces")
                       "TAB n" '(my/persp-switch :wk "New Workspace")
                       "TAB d" '(persp-kill :wk "Kill workspace")
                       "TAB 1" '(lambda () (interactive) (persp-switch-by-number 1) :wk "Switch to workspace 1")
                       "TAB 2" '(lambda () (interactive) (persp-switch-by-number 2) :wk "Switch to workspace 2")
                       "TAB 3" '(lambda () (interactive) (persp-switch-by-number 3) :wk "Switch to workspace 3")
                       "TAB 4" '(lambda () (interactive) (persp-switch-by-number 4) :wk "Switch to workspace 4")
                       "TAB 5" '(lambda () (interactive) (persp-switch-by-number 5) :wk "Switch to workspace 5")
                       ))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name t
		doom-modeline-display-default-persp-name t
		doom-modeline-buffer-encoding nil
		)
  )

(use-package dashboard
  :demand
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
        dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-startup-banner (concat (file-name-directory user-init-file) "banner.png")

        dashboard-center-content t)

  ;; Sets which dashboard items should show
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "rocket")
                                  (registers . "database")))
  (setq dashboard-projects-switch-function 'projectile-persp-switch-project
        dashboard-items '(
                          (recents . 5)
                          (projects . 10)
                          )
        dashboard-set-navigator t))

(use-package doom-themes
  :demand
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
      doom-modeline-enable-word-count t
      )
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(if (eq system-type 'windows-nt)
	(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-19"))
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-17"))
  )
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t)

(use-package display-line-numbers-mode
  :ensure nil
  :defer
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative
        display-line-numbers-current-absolute t))

(use-package ligature
  :hook (prog-mode . ligature-mode)
  (org-mode . ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 't '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
                                       "\\\\" "://")))

(use-package beacon
  :defer 2
  :config
  (setq beacon-blink-when-window-scrolls t)
  (add-to-list 'beacon-dont-blink-major-modes 'dashboard-mode )
  (beacon-mode 1))

(use-package hl-line
  :ensure nil
  :hook (prog-mode . hl-line-mode)
  (org-mode . hl-line-mode)
  )

(use-package centered-window
  :defer
  :hook
  (org-mode . centered-window-mode))

(use-package visual-line-mode
  :ensure nil
  :hook (org-mode . visual-line-mode))

(use-package toc-org
  :defer
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(use-package org-superstar
  :defer
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  :init
  (setq org-ellipsis "⤵"
        org-superstar-special-todo-items t
        org-superstar-todo-bullet-alist
        '(("TODO" . ?☐)
          ("DONE" . ?✔))))

(use-package org-tree-slide
  :defer
  :commands (org-tree-slide-mode)
  :bind
  ("<f8>" . org-tree-slide-move-previous-tree)
  ("<f9>" . org-tree-slide-move-next-tree)
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (plantuml . t)
   (C . t)
   (java . t)
   (python . t)))

(nconc org-babel-default-header-args:java
	 '((:dir . "/tmp/")))

(setq org-babel-default-header-args:js
             '((:exports . "both") (:results . "output")))

(setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-confirm-babel-evaluate nil)

(use-package org-auto-tangle
  :defer
  :hook (org-mode . org-auto-tangle-mode))

(setq org-directory "~/org")
(setq org-agenda-files '("Todos.org"))

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

(setq org-agenda-timegrid-use-ampm 1)

(use-package org-download)

(setq org-hide-emphasis-markers t)

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config (setq org-appear-autolinks t
                org-appear-autosubmarkers t
                org-appear-autoentities t
                org-appear-inside-latex t))

(setq org-return-follows-link t)

(setq org-startup-indented t
      org-startup-with-inline-images t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      org-image-actual-width '(300))

(use-package latex
  :hook (LaTeX-mode . (lambda ()
                        (setq TeX-command-extra-options "-shell-escape")
                        (electric-indent-local-mode -1)
                        (turn-on-reftex)
                        (prettify-symbols-mode)
                        ))
  :demand
  :ensure
  (auctex :build (:not elpaca--compile-info)
          :pre-build (("./autogen.sh")
                      ("./configure"
                       "--without-texmf-dir"
                       "--with-packagelispdir=./"
                       "--with-packagedatadir=./")
                      ("make"))
          :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
          :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :config
  (setq TeX-show-compilation t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list
   'TeX-view-program-selection
   '(output-pdf "PDF Tools"))
  )

;; (use-package evil-tex
;;   :hook
;;   (LaTeX-mode . evil-tex-mode))

(use-package pdf-tools
  :mode ("\\.pdf'" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode))

(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
             ((looking-at (concat re-end re-env "}"))
              indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq
    LaTeX-indent-level-item-continuation 2
    LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))

(use-package counsel
  :config (setq ivy-use-virtual-buffers t
                ivy-count-format "(%d/%d) ")
  (ivy-mode)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-c t" . counsel-load-theme)
  )

(defun +ys/compile-project ()
  (interactive)
  (call-interactively 'projectile-compile-project))

(use-package projectile
  :demand
  :general
  (ys/leader-keys
    "pc" '(+ys/compile-project :wk "Compile project"))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :general (ys/leader-keys
                       "/" '(counsel-projectile-grep :wk "Grep Project")
                       "bb" '(counsel-projectile-switch-to-buffer :wk "Project buffers")
                       "bB" '(counsel-switch-buffer :wk "Buffers"))
  :commands
  (counsel-projectile-find-file
   counsel-projectile-grep
   counsel-projectile-switch-project
   counsel-projectile-switch-to-buffer)
  :config
  (counsel-projectile-mode 1))

(defun ys/project-find-or-switch ()
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-find-file)
    (call-interactively 'projectile-persp-switch-project)))

(use-package persp-projectile 
  :general (ys/leader-keys
             "SPC" '(ys/project-find-or-switch :wk "Find file")
             "pp" '(projectile-persp-switch-project :wk "Switch project"))
  :after projectile)

(use-package transient)
(use-package seq)

(use-package magit
  :ensure (:wait t)
  :general (ys/leader-keys
             "g" '(:ignore t :wk "Magit")
             "gg" '(magit :wk "Open magit")
             "gp" '(magit-push :wk "Push commits"))
  :commands magit
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  )

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package neotree
  :defer
  :commands neotree-toggle
  :general (ys/leader-keys
             "n" '(neotree-toggle :wk "Neotree"))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
      neo-window-width 25
      neo-smart-open t
      neo-show-hidden-files t))

(use-package harpoon
  :general (ys/leader-keys
             "h" '(:ignore t :wk "Harpoon")
             "hh" '(harpoon-quick-menu-hydra :wk "Quick menu")
             "h1" '(harpoon-go-to-1 :wk "Go to 1")
             )
  )

(use-package doom-snippets 
  :after yasnippet
  :ensure (doom-snippets :type git :host github
						   :repo "doomemacs/snippets"
						   :files ("*.el" "*")))

(use-package yasnippet
  :hook (snippet-mode . (lambda ()
                          (setq mode-require-final-newline nil)))
  :defer 1
  :config
  (add-to-list 'yas-snippet-dirs '"~/dotfiles/home/dev/emacs/snippets")
  (setq yas-verbosity 2)
  (yas-global-mode 1)
  )

(use-package lsp-mode
  :general
  (ys/leader-keys
    "ca" '(lsp-execute-code-action :wk "Code actions")
    "lr" '(lsp-rename :wk "Rename reference"))
  :ensure (:wait t)
  :hook (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-inlay-hint-enable t
        lsp-enable-folding t
        lsp-enable-snippet t))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-java
  :config
  (setq c-basic-offset 2)
  (eldoc-mode -1)
  :hook (java-mode . (lambda ()
                       (require 'lsp-java) 
                       (defun lsp-java--ls-command ()
                         (list "jdtls"
                               "-configuration" (concat (getenv "HOME") "/.cache/jdtls/")
                               "-data" (concat (getenv "HOME") "/.jdtls")))
                       (setq lsp-java-server-install-dir (concat (getenv "JDTLS_PATH") "/"))
                       (lsp-deferred))))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

(use-package dap-mode
  :config
  (setq dap-auto-configure-mode t))

(use-package corfu
  :demand
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.5
        corfu-preview-current t
        corfu-min-width 50
        corfu-max-width corfu-min-width
        corfu-count 10
        corfu-scroll-margin 2
        )
  :bind (:map corfu-map ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("RET" . corfu-insert)
              )
  )

(use-package nerd-icons-corfu
  :demand
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(use-package cape
  :demand
  :init
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :after cape
  :init
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package apheleia
  :demand
  :config
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021" ))
  :general (ys/leader-keys
             "cf" '(apheleia-format-buffer
                    :wk "Format buffer")))

;; Indent blankline
(use-package highlight-indent-guides
  :defer
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'top)
  )

(use-package rainbow-delimiters
  :defer
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer
  :hook (prog-mode . rainbow-mode))

(if (not (eq system-type 'windows-nt))
    (use-package envrc
      :demand
      :init (envrc-global-mode))
)

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  )

(use-package rustic
  :config (setq rustic-rustfmt-args "--edition 2021"))

(use-package pyvenv)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :config (setq markdown-command "pandoc")
  )

(use-package arduino-mode
  :mode ("\\.ino\\'" . arduino-mode)
  )

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config (setq org-plantuml-executable-path (executable-find "plantuml")
                plantuml-executable-path (executable-find "plantuml")
                org-plantuml-exec-mode 'plantuml
                plantuml-default-exec-mode 'executable)
  )

(use-package tree-sitter
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package emacs
  :ensure nil
  :config
  (setq compilation-scroll-output t
        compilation-always-kill t
        )
  (local-set-key (kbd "q") 'kill-buffer-and-window)
  )

(if (not (eq system-type 'windows-nt))
  	(progn
	  (message "unix")
      (use-package vterm
		:defer 1)
      (use-package vterm-toggle
		:after vterm
		:general (ys/leader-keys
				   "t" '(vterm-toggle :wk "Vterm"))
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
  					   (window-height . 0.3)))))

  (progn
	(message "eshell")
	(use-package eshell
	  :ensure nil
	  :defer 1)
  	(use-package eshell-toggle
	  :custom
  	  (eshell-toggle-size-fraction 3)
	  :after eshell
  	  :config
	  :general (ys/leader-keys
				 "t" '(eshell-toggle :wk "Eshell"))
  	  )))

(use-package which-key
  :defer 1
  :config
  (which-key-mode 1)
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.1
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t))

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

(use-package flyspell-mode
  :ensure nil
  :hook (org-mode . flyspell-mode)
  )

(use-package flyspell-correct-ivy
  :after flyspell-mode
  :commands flyspell-correct-wrapper
)

(use-package sudo-edit)

(use-package emacs
  :ensure nil
  :general (ys/leader-keys
             "of" '(+macos/reveal-in-finder :wk "Open in finder")
             "op" '(+macos/reveal-project-in-finder :wk "Open in finder")
             )
  :if (eq system-type 'darwin)
  :config
  (setq ns-use-native-fullscreen nil
        mac-control-modifier 'control
        mac-command-modifier 'meta
        mac-right-option-modifier 'control)

  (defun +macos-open-with (&optional app-name path)
    "Send PATH to APP-NAME on OSX."
    (interactive)
    (let* ((path (expand-file-name
                  (replace-regexp-in-string
                   "'" "\\'"
                   (or path (if (derived-mode-p 'dired-mode)
                                (dired-get-file-for-visit)
                              (buffer-file-name)))
                   nil t)))
           (command (format "open %s"
                            (if app-name
                                (format "-a %s '%s'" (shell-quote-argument app-name) path)
                              (format "'%s'" path)))))
      (message "Running: %s" command)
      (shell-command command)))

  (defmacro +macos--open-with (id &optional app dir)
    `(defun ,(intern (format "+macos/%s" id)) ()
       (interactive)
       (+macos-open-with ,app ,dir)))

  (+macos--open-with reveal-in-finder "Finder" default-directory)
  (+macos--open-with reveal-project-in-finder "Finder" (or (projectile-project-root) default-directory))
  (+macos--open-with open-file nil buffer-file-name)

  )

;; Automatically reverts buffers for changed files
(global-auto-revert-mode 1)

;; Reverts dired as well
(setq global-auto-revert-non-file-buffers t)

;; Remembers the last place you visited in a file
(save-place-mode 1)

;; Disable unrelated warnings
(setq warning-minimum-level :error)

;; Disable lock file creation
(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Removes annoying prompts
(setq use-short-answers t)
