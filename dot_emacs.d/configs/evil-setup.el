(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :bind (("<escape>" . keyboard-escape-quit)
         ("s-k" . evil-window-delete))
  :config
  (setq evil-want-integration 1)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (evil-define-key '(normal visual) clojure-mode-map
    "H" 'clojure-backward-logical-sexp
    "L" 'clojure-forward-logical-sexp)
  (evil-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package treemacs-evil)

(use-package evil-org
  :hook org-mode
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
