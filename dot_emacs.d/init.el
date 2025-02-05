;; -*- lexical-binding: t; -*-
;; Emacs comes with package.el for installing packages.
;; Try M-x list-packages to see what's available.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/dev/crawlingchaos/editors/elisp")
(add-to-list 'load-path "~/.emacs.d/configs")

(defvar addons
  '("ux.el"
    "witchhazel-theme.el"
;;    "terminals.el"
    "projects.el"
    "filetree.el"
    "shell-integration.el"
    "setup-clojure.el"
    "editing.el"
    "git.el"
    "web.el"
    "markdown.el"
    "yaml.el"
    "evil-setup.el"
    "org-setup.el"
    "leaderkeys.el"
    "misc.el"
    ))

(dolist (x addons)
  (load x))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; If you remove this, the theme will be doom-dracula
(enable-theme 'witchhazel)
