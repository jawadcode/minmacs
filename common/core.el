;; Core Configurations -*- lexical-binding: t; -*-

;; === USER INTERFACE TWEAKS ===

(set-face-font 'default
               (font-spec :family "Iosevka Term SS07"
                          :size 20
                          :weight 'normal
                          :width 'normal
                          :slant 'normal))
(set-face-font 'fixed-pitch
               (font-spec :family "Iosevka Term SS07"
                          :size 20
                          :weight 'normal
                          :width 'normal
                          :slant 'normal))
(set-face-font 'variable-pitch
               (font-spec :family "Roboto"
                          :size 20
                          :weight 'normal
                          :width 'normal))

(if (eq system-type 'windows-nt)
    (when (member "Noto Emoji" (font-family-list))
      (set-fontset-font t
                        'emoji
                        (font-spec :family "Noto Emoji"
                                   :size 20
                                   :weight 'normal
                                   :width 'normal
                                   :slant 'normal)))
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t
                        'emoji
                        (font-spec :family "Noto Color Emoji"
           :size 20
                                   :weight 'normal
                                   :width 'normal
                                   :slant 'normal))))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'image-mode-hook #'(lambda () (blink-cursor-mode -1)))

(window-divider-mode)
(global-hl-line-mode)

;; === INITIALISE STRAIGHT.EL ===

(setq straight-base-dir (file-name-parent-directory user-emacs-directory))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-always-ensure t
      use-package-compute-statistics t)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; === CORE PACKAGES ===

(use-package standard-themes
  :custom
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-disable-other-themes t)
  (standard-themes-mixed-fonts t)
  (standard-themes-variable-pitch-ui t)
  (standard-themes-prompts '(extrabold italic))
  (standard-themes-headings
    '((0 . (variable-pitch light 1.9))
    (1 . (variable-pitch light 1.8))
    (2 . (variable-pitch light 1.7))
    (3 . (variable-pitch semilight 1.6))
    (4 . (variable-pitch semilight 1.5))
    (5 . (variable-pitch 1.4))
    (6 . (variable-pitch 1.3))
    (7 . (variable-pitch 1.2))
    (agenda-date . (1.3))
    (agenda-structure . (variable-pitch light 1.8))
    (t . (variable-pitch 1.1))))
  :config (standard-themes-load-light))

(use-package mood-line
  :config (mood-line-mode))

(use-package gcmh
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :config (gcmh-mode 1))

;; === EVIL MODE + WHICH-KEY ===

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-shift-width 4)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :custom (evil-collection-mode-list '(dashboard dired ibuffer))
  :config (evil-collection-init))

(use-package evil-anzu)

(use-package which-key
  :custom
  (which-key-idle-delay 0.05)
  (which-key-add-column-padding 0)
  (which-key-show-docstrings t)
  (which-key-max-description-length 54)
  (which-key-allow-evil-operator t)
  :config (which-key-mode 1))

;; === SETTING UP THE PATH/ENVIRONMENT (WINDOWS ONLY) ===

(setq env-file-path (concat (getenv "HOME") "env.el"))

(defun gen-env-file ()
  (interactive)
  (let ((dirname (file-name-directory env-file-path)))
    (make-directory dirname t))
  (with-temp-file env-file-path
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; This file was automatically generated and will be overwritten.\n")
    (insert (pp-to-string process-environment))))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name
        (seq-some (lambda (x) (if (file-exists-p x) x nil))
                  (list "C:/Program Files/PowerShell/7/pwsh.exe")))

  (if (null (file-exists-p env-file-path)) (gen-env-file))

  (with-temp-buffer
    (insert-file-contents env-file-path)
    (when-let (env (read (current-buffer)))
      (let ((tz (getenv-internal "TZ")))
        (setq-default
         process-environment
         (append env (default-value 'process-environment))
         exec-path
         (append (split-string (getenv "PATH") path-separator t)
                 (list exec-directory))
         shell-file-name
         (or (getenv "SHELL")
             (default-value 'shell-file-name)))
        (when-let (newtz (getenv-internal "TZ"))
          (unless (equal tz newtz)
            (set-time-zone-rule newtz))))
      env)))

;; === PROGRAMS ===

(use-package dashboard
  :init (setq initial-buffer-choice 'dashboard-open)
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items))
  (dashboard-items '((recents . 10)
                     (projects . 10)))
  :hook (special-mode . (lambda () (setq-local centaur-tabs-mode -1)))
  :config
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-change-fonts (face-attribute 'variable-pitch :font) 160)
  (centaur-tabs-mode 1)
  :hook (dashboard-mode . centaur-tabs-local-mode)
  :bind ( :map centaur-tabs-mode-map
          ("<leader> t n" . centaur-tabs-forward)
          ("<leader> t p" . centaur-tabs-backward)))

(use-package all-the-icons)

(use-package treemacs-all-the-icons
  ;; :defer t
  :commands treemacs-all-the-icons)

(use-package treemacs
  :config
  ;; (treemacs-load-all-the-icons-with-workaround-font "Hermit")
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  :bind ("<leader> r" . treemacs))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar :after treemacs)

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

;; === KEY BINDINGS ===

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)

(defvar-keymap jawad/buffer-map
  :doc "My buffer keymap"
  "k" #'kill-buffer)

(defvar-keymap jawad/window-map
  :doc "My window keymap"
  "x" #'evil-window-delete
  "n" #'evil-window-new
  "m" #'evil-window-vnew
  "h" #'evil-window-split
  "v" #'evil-window-vsplit)

(use-package f)

(setq config-dir (f-dirname
                  (file-truename
                   (f-join user-emacs-directory "init.el"))))

(defun open-configs ()
  "Open config files."
  (interactive)
  (find-file (f-join config-dir "early-init.el"))
  (find-file (f-join config-dir "init.el")))

(defvar-keymap jawad/file-map
  :doc "My file keymap"
  "f" #'consult-fd
  "r" #'consult-recent-file
  "g" #'consult-ripgrep
  "c" #'open-configs)

(defvar-keymap jawad/global-map
  :doc "My global keymap"
  "f" jawad/file-map
  "b" jawad/buffer-map
  "w" jawad/window-map
  "p" project-prefix-map
  "h" help-map)

(evil-define-key 'normal 'global (kbd "<leader>") jawad/global-map)

;; === MINIBUFFER CONFIGURATION ===

(use-package vertico
  :demand t
  :custom
  (vertico-cycle 1)
  (vertico-resize nil)
  :config (vertico-mode 1)
  :bind ( :map vertico-map
          ("M-j" . vertico-next)
          ("M-k" . vertico-previous)))

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

(use-package orderless
  :demand t
  :config (setq completion-styles '(orderless basic)))

(use-package consult
  :demand t
  :config (setq completion-in-region-function 'consult-completion-in-region))

;; === EDITOR CONFIGURATION ===

(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :delight)

(use-package tree-sitter-langs)

(setq default-directory (f-slash (getenv "HOME")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq custom-file (f-join user-emacs-directory "custom.el"))

(setq-default indent-tabs-mode -1)
(setq-default tab-width 4)

(electric-pair-mode t)

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
  (global-ligature-mode 1))

;; === COMPLETIONS & SNIPPETS ===

(use-package company
  :init (setq company-tooltip-align-annotations t)
  :bind ( :map company-active-map
          ("M-j"   . #'company-select-next)
          ("M-k"   . #'company-select-previous)
          ("<tab>" . #'company-complete-selection))
  :config (global-company-mode))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :commands yas-minor-mode)

(use-package yasnippet-snippets
  :after (yasnippet))

;; === DIRENV (FOR NIX) ===

(use-package inheritenv)

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :bind ( :map envrc-mode-map
          ("<leader> e" . envrc-command-map)))
