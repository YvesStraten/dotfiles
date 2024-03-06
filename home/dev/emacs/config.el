;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 21 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono NF" :size 19))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(use-package! elcord
  :hook (doom-first-buffer . elcord-mode)
  :config
  (setq elcord-editor-icon 'emacs_icon))

(use-package! beacon
  :config
  (setq beacon-dont-blink-major-modes
        (append '(vterm-mode +doom-dashboard-mode dap-mode pdf-view-mode) beacon-dont-blink-major-modes))
  (beacon-mode))

(map! :leader
      :desc "Kill buffer" "x" #'centaur-tabs--kill-this-buffer-dont-ask
      :desc "Kill all other buffers" "X" #'centaur-tabs-kill-other-buffers-in-current-group
      )

(map! :leader
      :desc "Look definition" "ld" #'lsp-find-references
      :desc "Format" "lf" #'lsp-format-buffer
      :desc "Rename" "lr" #'lsp-rename)

(map! :leader
      :desc "Split" "sh" #'split-window-horizontally
      :desc "vsplit" "sv" #'split-window-vertically)

(after! neotree
  (use-package! neotree
    :config
    (setq neo-smart-open 1
        neo-window-width 20
        neo-autorefresh 1
        neo-show-hidden-files 1)
  :bind (:map evil-normal-state-map
              ("C-n" . neotree-toggle))))

(use-package! rainbow-mode
  :hook (doom-first-buffer . rainbow-mode))

;; (use-package! xenops
;;   :hook (LaTeX-mode . xenops-mode))

(setq TeX-command-extra-options "-shell-escape")
(setq shell-escape-mode "-shell-escape")
(setq-default TeX-master nil)

(after! pdf-tools
  (use-package! pdf-tools
    :hook (pdf-view-mode . pdf-view-midnight-minor-mode)))

(after! centaur-tabs
  (use-package! centaur-tabs
    :bind
    (:map evil-normal-state-map
          ("C-1" . centaur-tabs-forward)
          ("C-2" . centaur-tabs-backward)))

  (use-package! pdf-tools
    :hook
    (pdf-view-mode . centaur-tabs-local-mode)
    (LaTeX-mode . centaur-tabs-local-mode)
    (dired-mode . centaur-tabs-local-mode)
    ))

(after! tex
  (use-package! tex
    :config
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    :hook
    (LaTeX-mode .
                (lambda ()
                  (add-hook 'after-save-hook
                              (lambda ()
                                (setq-local split-height-threshold 90)
                                (setq-local split-width-threshold 60)
                                  (TeX-save-document (TeX-master-file))
                                  (TeX-command-run-all nil))
                              0 t)))))

;; (after! dired
;;   (use-package! dirvish
;;     :hook (dirvish-side . dirvish-side-follow-mode)
;;     :init (dirvish-override-dired-mode)
;;     :config
;;     (setq dirvish-attributes
;;           '(vc-state subtree-state all-the-icons
;;             collapse git-msg file-time file-size))
;;     (setq dired-mouse-drag-files t
;;           mouse-drag-and-drop-region-cross-program t)
;;     :bind
;;     (:map evil-normal-state-map
;;           ("C-n" . dirvish-side)
;;      :map dirvish-mode-map
;;           ("y" . dirvish-yank-menu)
;;           ("TAB" . dirvish-subtree-toggle))))

(defun startup-with-elcord ()
  (interactive)
  (if (yes-or-no-p "Do you want to start with elcord?")
      (cl-return)
    (remove-hook! 'doom-first-buffer-hook 'elcord-mode)))
