;; Initialisation -*- lexical-binding: t; -*-

(let ((emacs-dir (file-name-parent-directory user-emacs-directory)))
  (load (file-name-concat emacs-dir "common/core.el"))
	(load (file-name-concat emacs-dir "common/rest.el")))

;; === LSP & LANGUAGE CONFIGURATIONS ===

(use-package lsp-mode
	:custom
	(lsp-enable-which-key-integration t)
	(lsp-inlay-hint-enable t)
  :hook ((c-mode   . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map))

(use-package lsp-ui)
(use-package lsp-ivy)

(use-package lean4-mode
  :straight (lean4-mode
             :host github
             :repo "leanprover/lean4-mode"
             :files ("*.el" "data"))
  :commands lean4-mode)
