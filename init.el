;; Initialisation -*- lexical-binding: t; -*-

;; === SET FONTS ===

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
													:size 16
													:weight 'normal
													:width 'normal))
(set-fontset-font t
									'emoji
									(font-spec :family "Noto Color Emoji"
														 :size 20
														 :weight 'normal
														 :width 'normal
														 :slant 'normal))

;; === LOAD ELPACA ===

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

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; === CORE PACKAGES ===

(use-package delight
  :ensure (:wait t)
  :demand t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (abbrev-mode nil "abbrev")
             (flycheck-mode nil "flycheck"))))

(use-package f
  :ensure (:wait t)
  :demand t)

(use-package evil
	:ensure (:wait t)
	:demand t
	:custom
	(evil-want-integration t)
	(evil-want-keybinding nil)
	(evil-vsplit-window-right t)
	(evil-split-window-below t)
	:config
	(evil-set-undo-system 'undo-redo)
	(evil-mode 1)
	(evil-set-leader 'normal (kbd "SPC"))
	(defvar-keymap jawad/window-map
		:doc "My window map"
		"x" #'evil-window-delete
		"n" #'evil-window-new
		"m" #'evil-window-vnew
		"h" #'evil-window-split
		"v" #'evil-window-vsplit)
	:config (evil-define-key 'normal 'global (kbd "<leader>w") jawad/window-map))

;; === SET DIRECTORIES ===

(setq default-directory (f-slash (getenv "HOME")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq custom-file (f-join user-emacs-directory "custom.el"))

;; === KEYBINDS ===

(use-package which-key
  :custom
  (which-key-add-column-padding 3)
  (which-key-idle-delay 0.1)
  :config (which-key-mode 1)
  :delight)

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)

;; === EDITOR ===

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package emacs
	:ensure nil
  :delight (visual-line-mode))

(setq-default indent-tabs-mode -1)
(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(defun smol-tabs ()
	(setq tab-width 2)
	(setq-local evil-shift-width 2))

(add-hook 'emacs-lisp-mode-hook #'smol-tabs)

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config (require 'smartparens-config)
	:delight)

;; (use-package tree-sitter
;; 	:after tree-sitter-langs
;; 	:demand t
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;   :delight)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; === DIRENV ===

(use-package envrc
	:ensure (:wait t)
	:demand t
	:hook (after-init . envrc-global-mode)
	;; :config (evil-define-key 'normal envrc-mode-map (kbd "<leader>e") #'envrc-command-map))
	:bind ( :map envrc-mode-map
					("<leader>e" . envrc-command-map)))

(use-package inheritenv)

;; === COMPLETIONS ===

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  :delight)

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :delight)

(use-package ivy-rich
  :after ivy
  :custom
  ;; I'll be honest, idk what this does
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transform)
  (ivy-rich-mode 1))

(use-package company
  :init (setq company-tooltip-align-annotations t)
  :config
  (keymap-set company-active-map "C-n"   nil)
  (keymap-set company-active-map "C-p"   nil)
  (keymap-set company-active-map "RET"   nil)
  (keymap-set company-active-map "M-j"   #'company-select-next)
  (keymap-set company-active-map "M-k"   #'company-select-previous)
  (keymap-set company-active-map "<tab>" #'company-complete-selection)
  (global-company-mode)
  (delight 'company-capf-mode)
  :delight)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :delight)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :delight yas-minor-mode
	:commands yas-minor-mode)

(use-package yasnippet-snippets
  :after (yasnippet))

;; === LSP ===

(use-package eglot
	:custom (eglot-report-progress t)
	:ensure nil
	:hook ((python-mode . eglot-ensure))
	:config
	(if (eq system-type 'gnu/linux)
			(progn
				(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
				(add-hook 'nix-mode-hook #'eglot-ensure))))

(use-package rust-mode
	:hook ((rust-mode . eglot-ensure)
				 (rust-mode . prettify-symbols-mode))
	:commands rust-mode)

(defun c/++-setup ()
	(setq c-basic-offset tab-width)
	(eglot-ensure))

(add-hook 'c-mode-hook   #'c/++-setup)
(add-hook 'c++-mode-hook #'c/++-setup)

(use-package tuareg
  :hook ((tuareg-mode . eglot-ensure)
				 (tuareg-mode . prettify-symbols-mode)
				 (tuareg-mode . (lambda () (setq tuareg-mode-name "🐫"))))
	:commands tuareg-mode)

(use-package haskell-mode
	:hook ((haskell-mode . eglot-ensure)
				 (haskell-literate-mode . eglot-ensure)
				 (haskell-mode . smol-tabs))
	:commands (haskell-mode haskell-literate-mode))

(use-package glsl-mode
	:hook (glsl-mode . eglot-ensure)
	:commands glsl-mode)

(use-package meson-mode
  :hook (meson-mode . eglot-ensure)
  :commands meson-mode)

(use-package cmake-mode
  :hook (cmake-mode . eglot-ensure)
  :commands cmake-mode)

(use-package typescript-mode
  :init (add-hook 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))
  :hook
  (javascript-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
	:commands (javascript-mode typescript-mode))

(use-package svelte-mode
  :hook ((svelte-mode . eglot-ensure)
         ;; Looks worse with TS, embedded CSS and JS isn't highlighted
         (svelte-mode . (lambda () (tree-sitter-hl-mode -1))))
	:commands svelte-mode)

(use-package lean4-mode
	:elpaca (lean4-mode
					 :host github
					 :repo "leanprover/lean4-mode"
					 :files ("*.el" "data"))
	:hook (lean4-mode . eglot-ensure)
	:commands lean4-mode)

(if (eq system-type 'gnu/linux)
	 (progn
		 (use-package idris2-mode
			 :elpaca (idris2-mode
								:host github
								:repo "idris-community/idris2-mode")
			 :hook (idris2-mode . eglot-ensure)
			 :commands idris2-mode)

		 (use-package nix-mode)))
