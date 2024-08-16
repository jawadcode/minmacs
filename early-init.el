;; Early Initialisation -*- lexcal-binding: t; -*-

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq package-enable-at-startup nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings nil
      inhibit-startup-screen t
      warning-minimum-level :error)

(set-language-environment "UTF-8")
(setq default-input-method nil) ; Undoes `set-language-environment`'s changes
(if (not (eq system-type 'windows-nt))
    (setq default-input-method 'utf-8)
  ;; Windows uses UTF-16 for its clipboard, so let emacs do its thing
  )
