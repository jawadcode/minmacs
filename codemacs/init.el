;; Initialisation -*- lexical-binding: t; -*-

(let ((emacs-dir (file-name-parent-directory user-emacs-directory)))
  (load-file (concat emacs-dir "common/core.el")))

;; === LSP & LANGUAGE CONFIGURATIONS ===

(use-package eglot
  :straight (:type built-in)
  :custom (eglot-report-progress t)
  :config
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil" :initializationOptions
                             (:nil (:formatting (:command ["alejandra"]))))))
  :hook ((python-mode . eglot-ensure)
         (conf-toml-mode . eglot-ensure)
         (nix-mode . eglot-ensure))
  :bind ( :map eglot-mode-map
          ("<leader> l r" . eglot-rename)
          ("<leader> l f" . eglot-format)
          ("<leader> l c" . eglot-code-actions)))

(use-package eldoc-box :hook (eldoc-mode . eldoc-box-hover-at-point-mode))

(defun smol-tabs ()
  (setq tab-width 2)
  (setq-local evil-shift-width 2))

(add-hook 'emacs-lisp-mode-hook #'smol-tabs)

(use-package rust-mode
  :hook (rust-mode . eglot-ensure)
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
  (js-mode . eglot-ensure)
  (javascript-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  :commands (javascript-mode typescript-mode))

(use-package svelte-mode
  :hook (svelte-mode . (lambda ()
                         (tree-sitter-hl-mode -1)
                         (eglot-ensure)))
  :commands svelte-mode)

(when (eq system-type 'gnu/linux)
  (use-package idris2-mode
    :straight (idris2-mode
             :host github
             :repo "idris-community/idris2-mode")
    :hook (idris2-mode . eglot-ensure)
    :commands idris2-mode)
  (use-package nix-mode :mode "\\.nix\\'"))

