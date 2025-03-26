(use-package lsp-mode
  :bind ("M-<f7>" . lsp-find-references)
  :hook clojure-mode)

(use-package lsp-ivy)

(use-package lsp-treemacs)

(use-package clojure-mode
  :bind ("M-s-l" . lsp-format-buffer))

(use-package paredit
  :hook (clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode scheme-mode))

(use-package cider
  :config
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t))

(add-hook 'before-save-hook #'lsp-format-buffer)

(use-package company
  :hook (cider-mode cider-repl-mode))

(use-package cider-hydra
  :hook clojure-mode)

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package jet)

(use-package neil
  :defer t
  :ensure t
  :custom
  (neil-prompt-for-version-p nil)
  (neil-inject-dep-to-project-p t))

(defun jet-json-to-clipboard ()
  (interactive)
  (jet-to-clipboard (jet--thing-at-point) '("-k" "--from=json" "--to=edn")))

(use-package quick-peek)

(defun clj-switch-profile ()
  "Quit the current CIDER session and restart with a universal argument."
  (interactive)
  (when (cider-connected-p)
    (cider-quit))
  (let ((current-prefix-arg '(4)))  ;; Simulate C-u (universal argument)
    (call-interactively #'cider-jack-in-clj)))

