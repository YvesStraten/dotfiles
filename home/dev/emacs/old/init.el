(defvar elpaca-installer-version 0.7)
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
  (setq use-package-always-defer t)

(defun reload-init-file ()
  "Reload the `init.el` configuration file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(use-package bug-hunter)

(use-package esup)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure (:wait t)
  :init (exec-path-from-shell-initialize)
  )

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
    "c" '(centaur-tabs-ace-jump :wk "Jump to tab"))

  (ys/leader-keys
    "l" '(:ignore t :wk "Lsp")
    "lr" '(lsp-rename :wk "Rename reference")
    "lf" '(format-all-buffer :wk "Formats buffer")
    "la" '(lsp-code-actions-at-point :wk "Code actions"))

  (ys/leader-keys
    "o" '(:ignore t :wk "Org")
    "ob" '(org-mark-ring-goto :wk "Travel to origin link")
    "oa" '(org-agenda :wk "Org agenda")
    "oe" '(org-export-dispatch :wk "Org export")
    "oi" '(org-toggle-item :wk "Org toggle Item")
    "ot" '(org-todo :wk "Org Todo")
    "oT" '(org-todo-list :wk "Org Todo List")
    "op" '(org-tree-slide-mode :wk "Present")))

(defun my/persp-switch (name)
  (interactive "s")
  (persp-switch name)
  (switch-to-buffer "*dashboard*")
  )

(use-package perspective
  :general (ys/leader-keys
    		       "TAB" '(:ignore t :wk "Workspaces")
    		       "TAB n" '(my/persp-switch :wk "New Workspace")
    		       "TAB d" '(persp-kill :wk "Kill workspace")
    		       "TAB 1" '(lambda () (interactive) (persp-switch-by-number 1) :wk "Switch to workspace 1")
    		       "TAB 2" '(lambda () (interactive) (persp-switch-by-number 2) :wk "Switch to workspace 2")
    		       "TAB 3" '(lambda () (interactive) (persp-switch-by-number 3) :wk "Switch to workspace 3")
    		       "TAB 4" '(lambda () (interactive) (persp-switch-by-number 4) :wk "Switch to workspace 4")
    		       "TAB 5" '(lambda () (interactive) (persp-switch-by-number 5) :wk "Switch to workspace 5")
    		       )
  :init (persp-mode))

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
        dashboard-startup-banner (concat (file-name-directory user-init-file) "marivector.png")

        dashboard-center-content t)

  ;; Sets which dashboard items should show
  (setq dashboard-banner-logo-title ""
        dashboard-set-footer nil
        dashboard-projects-switch-function 'counsel-projectile-switch-project
        dashboard-items '()
        dashboard-set-navigator t)

  (setq dashboard-navigator-buttons
        `(
          ;; First row
          ((nil
            "Edit emacs config"
            "Open the config file for emacs"
            (lambda (&rest _) (find-file "~/dotfiles/home/dev/emacs/old/README.org")
              )
            'default)
           (nil
            "Open Notes"
            "Open my notes"
            (lambda (&rest _) (org-roam-node-find))
            'default)
           )

          ;; Second row
          ((nil
            "Todo list"
            "Open todo list"
            (lambda (&rest _) (find-file "~/org/Todos.org"))
            'default)
           ))))

;; (setq dashboard-set-file-icons t)
;; (setq dashboard-set-heading-icons t)
;; (setq dashboard-display-icons-p t
;;       dashboard-icon-type 'all-the-icons)
;; (setq dashboard-heading-icons '((recents   . "history")
;;                                 (bookmarks . "bookmark")
;;                                 (agenda    . "calendar")
;;                                 (projects  . "rocket")
;;                                 (registers . "database"))))

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
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-20"))
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

(use-package elcord
  :defer 2
  :config (elcord-mode)
  ;; (setq elcord-editor-icon 'emacs_icon)
  )

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

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(if (eq system-type 'darwin)
      (progn
  	(message "Is darwin")
  	(tool-bar-mode t)
  	(menu-bar-mode t)))

(set-frame-parameter nil 'alpha-background 70) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 70)) ; For all new frames henceforth

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

(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook (lambda () (electric-indent-local-mode -1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook (lambda () (prettify-symbols-mode 1)))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list
   'TeX-view-program-selection
   '(output-pdf "PDF Tools"))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (setq-local split-height-threshold 90)
                          (setq-local split-width-threshold 60)
                          (TeX-save-document (TeX-master-file))
                          (TeX-command-run-all nil))
                        0 t))))


(use-package latex
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
  )

(define-derived-mode my-tex-mode LaTeX-mode
  "my-tex-mode"
  "My customised tex mode"
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-save-query nil
        TeX-source-correlate-start-server t
        TeX-command-extra-options "-shell-escape")
  (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
  (setq TeX-lisp-directory TeX-data-directory)
  )

(use-package my-tex-mode
  :ensure nil
  :mode "\\.tex'")

(use-package evil-tex
  :hook
  (LaTeX-mode . evil-tex-mode))

(use-package pdf-tools
  :demand
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (pdf-loader-install))

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
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))

(use-package projectile
  :defer 1
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :general (ys/leader-keys
  		       "SPC" '(counsel-projectile-find-file :wk "Find file")
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

(use-package magit
  :ensure (:wait t)
  :general (ys/leader-keys
             "g" '(:ignore t :wk "Magit")
             "gg" '(magit :wk "Open magit")
             "gp" '(magit-push :wk "Push commits"))
  :commands magit)

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :hook (dirvish-side . dirvish-side-follow-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
       ("d" "~/Downloads" "Downloads")
       ))
  :bind
  (:map evil-normal-state-map
  	      ("C-n" . dirvish-side))
  (:map dirvish-mode-map
      ("q" . dirvish-quit)
      ("a" . dirvish-quick-access)
      ("TAB" . dirvish-subtree-toggle)
      )
  :config (setq dirvish-attributes
  			      '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size)))

(use-package centaur-tabs
  :hook (dashboard-mode . centaur-tabs-local-mode) 
  (calendar-mode . centaur-tabs-local-mode)
  (eshell-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  (pdf-view-mode . centaur-tabs-local-mode)
  (magit-mode . centaur-tabs-local-mode)
  (org-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-height 40
        centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-modified-marker t))

(use-package counsel
  :commands (counsel-M-x
             counsel-find-file
             counsel-describe-variable
             counsel-load-theme)
  :config (setq ivy-use-virtual-buffers t
                ivy-count-format "(%d/%d) ")
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-c t" . counsel-load-theme)
  )

(use-package doom-snippets 
  :after yasnippet
  :ensure (doom-snippets :type git :host github
  						 :repo "doomemacs/snippets"
  						 :files ("*.el" "*")))

(use-package yasnippet
  :defer 1
  :config
  (add-to-list 'yas-snippet-dirs '"~/dotfiles/home/dev/emacs/snippets")
  (setq yas-verbosity 2)
  (yas-global-mode 1)
  )

(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-inlay-hint-enable t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-snippet t))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package dap-mode
  :config
  (setq dap-auto-configure-mode t))

(use-package company
  :config
  (global-set-key (kbd "C-c y") 'company-yasnippet)
  :init
  (setq company-auto-commit nil
      company-minimum-prefix-length 2
      company-tooltip-limit 14
      company-tooltip-align-annotations t
      company-require-match 'never
      company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend)
      )
  (global-company-mode))

(use-package company-box
  :config
  (setq company-box-show-single-candidate t
      company-box-backends-colors nil)
  :hook (company-mode . company-box-mode))

(use-package company-math
  :after company
  :config
  (defun my-latex-mode-setup ()
      (setq-local company-backends
  			      (append '((company-math-symbols-latex company-latex-commands))
  					      company-backends)))
  (add-hook 'TeX-mode-hook 'my-latex-mode-setup)
  )

(use-package company-auctex
  :after company
  :config (company-auctex-init))

(use-package apheleia
  :init (apheleia-global-mode +1))

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

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(if (not (eq system-type 'windows-nt))
    (use-package direnv
      :hook (prog-mode . direnv-mode)
      ))

(use-package typescript-mode)
(use-package web-mode
  :config (setq web-mode-indent-style 4
                web-mode-css-indent-offset 4
                web-mode-code-indent-offset 4
                web-mode-attr-indent-offset 4
                web-mode-script-padding 4)
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  )

(use-package rustic)

(use-package python-mode)

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
        which-key-idle-delay 0.8
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

(use-package emacs
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (setq ns-use-native-fullscreen nil
        mac-control-modifier 'control
        mac-command-modifier 'meta
        mac-right-option-modifier 'control))

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
