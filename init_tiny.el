;; The default is 800 kbytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;; this code is exactly what setup.el does,
;; but we can't use setup to setup itself!
(if (package-installed-p 'setup)
    nil
  (if (memq 'setup package-archive-contents)
      nil
    (package-refresh-contents))
  (package-install 'setup))

(require 'setup)

(defun my/hook (hook)
  "Create an org-link target string using `hook://` url scheme."
  (shell-command (concat "open \"" hook "\"")))


(setq frame-title-format '((:eval buffer-file-name)))
;; diminish keeps minor modes from adding noise to the modeline
(setup (:package diminish))

;; which-key shows all the commands available after you type
;; a command prefix
(setup (:package which-key)
  (diminish 'which-key-mode)
  (which-key-mode)
  (:option which-key-idle-delay 0.3))

;; ivy is the completion framework. This makes M-x much
;; more usable.
;; installing counsel brings ivy and swiper as dependencies
(setup (:package counsel)
  (diminish 'ivy-mode)
  (ivy-mode)
  (:option ivy-use-virtual-buffers t
           ivy-count-format "%d/%d ")
  (:global "C-s" swiper
           "s-f" swiper
           "M-x" counsel-M-x))

(server-start)
;; projects with git and clojure
(setup (:package projectile)
  (projectile-mode +1)
  (:bind "C-c p" projectile-command-map))

(setup (:package counsel-projectile))

(setup (:package treemacs)
  (:global "<f8>" treemacs))

(setup (:package magit)
  (:global "C-M-;" magit-status))

(setup (:package magit-todos))

(setup (:package treemacs-projectile))

(setup (:package treemacs-magit))

(setup (:package paredit)
  (:global "C-SPC" completion-at-point
           "s-/" comment-region)
  (:hook-into emacs-lisp)
  (:bind "C-)" paredit-forward-slurp-sexp
         "C-(" paredit-backward-slurp-sexp
         "C-}" paredit-forward-barf-sexp
         "C-{" paredit-backward-barf-sexp))

(setup (:package yasnippet)
  (:option yas-global-mode 1))

(setup (:package lsp-mode)
  (:option lsp-enable-which-key-integration t))

(setup (:package clojure-mode)
  (:file-match "\\.clj[sc]?\\'")
  (:hook enable-paredit-mode
         lsp))

(setup (:package clj-refactor)
  (:option cljr-warn-on-eval nil))

(setup (:package yaml-mode))

(setup (:package nix-mode))

(setup (:package rainbow-delimiters)
  (:hook-into prog-mode))

;; without this, emacs doesn't have leiningen on its path
(setup (:package exec-path-from-shell)
  (exec-path-from-shell-initialize))

(setup (:package cider)) 

;; company provides auto-completion for CIDER
;; see https://docs.cider.mx/cider/usage/code_completion.html
(setup (:package company)
  (:hook-into cider-mode
              cider-repl-mode))

(setup (:package cider-hydra)
  (:hook-into clojure-mode))
;; bbatsov made some really useful functions
(setup (:package crux)
  (:global "M-o" crux-other-window-or-switch-buffer
           "s-r" crux-recentf-find-file
           "C-c t" crux-visit-term-buffer
           "s-<backspace>" crux-kill-whole-line
           "s-d" crux-duplicate-current-line-or-region))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; terraform all the things
(setup (:package terraform-mode)
  (:hook terraform-format-on-save-mode
         lsp))

;; cosmetic, but nice
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable tooltips
(set-fringe-mode 10)          ; Give some breathing room
(add-to-list 'default-frame-alist '(height . 77))
(add-to-list 'default-frame-alist '(width . 148))

(set-face-attribute 'default nil :font "Fira Code" :height 140)

(setup (:package doom-modeline)
  (doom-modeline-mode t))

(setup (:package doom-themes)
  (load-theme 'doom-laserwave t))

(setup (:package golden-ratio))

;; org-mode. This is where the tiny config stops being tiny.
(setup (:package org-bullets))

(setup (:package org)
  (:hook org-bullets-mode
         turn-on-auto-fill)
  (:global "<f12>" org-agenda)
  (:option org-src-fontify-natively t
           org-use-fast-todo-selection t
           org-image-actual-width nil
           org-M-RET-may-split-line nil
           org-return-follows-link t
           org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
           org-agenda-files (quote ("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org"))))

(setup (:package org-roam)
  (org-roam-db-autosync-mode)
  (:option org-roam-directory (file-truename "~/org-roam")
           org-roam-completion-everywhere t
           org-roam-dailies-directory "daily/"
           org-roam-capture-templates
          '(("d" "default" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
             :unarrowed t)
            ("m" "meeting" plain
             (file "~/org-roam/templates/MeetingTemplate.org")
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: Meetings\n")
             :unarrowed t)
            ("b" "book" plain "* Book Info\n\n- Author: %^{Author}\n\n* Notes\n\n  %?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :Books: %^{Fiction:?}\n")
             :unarrowed t))
           org-roam-v2-ack t))

;; this does full-text indexing of the org-roam files
(setup (:package deft)
  (:option deft-recursive t
           deft-use-filter-string-for-filename t
           deft-default-extension "org"
           deft-directory (file-truename "~/org-roam")))

(setup (:package org-re-reveal)
  (require 'org-re-reveal)
  (:option org-re-reveal-root "file:///usr/local/lib/node_modules/reveal.js"))

;; and here goes evil mode, folks
(setup (:package undo-tree)
  (global-undo-tree-mode 1))

(setup (:package evil)
  (:global "<escape>" keyboard-escape-quit)
  (:option evil-want-integration 1
           evil-want-keybinding nil
           evil-want-C-u-scroll t
           evil-want-C-i-jump nil
           evil-respect-visual-line-mode t
   evil-undo-system 'undo-tree)
  (evil-mode 1))

(setup (:package evil-collection)
  ;; Is this a bug in evil-collection?
  (setq evil-collection-company-use-tng nil)
  (:option evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))

(setup (:package treemacs-evil))

(setup (:package evil-org)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (:hook-into org))

(setup (:package general)
  (:option general-evil-setup t)
  (general-create-definer zt/leader-key-def
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-M-SPC"))

(zt/leader-key-def
  "TAB" 'crux-switch-to-previous-buffer
  "/" 'counsel-projectile-rg
  "a" 'avy-goto-word-or-subword-1
  "b" '(:ignore t :which-key "buffers")
  "bb" 'counsel-switch-buffer
  "bk" 'kill-buffer
  "c" '(:ignore t :which-key "clojure")
  "cc" 'cider-repl-clear-buffer
  "cj" 'cider-jack-in-clj
  "cg" 'evil-goto-definition
  "cu" 'cljr-find-usages
  "cb" 'cider-eval-buffer
  "cd" 'cider-hydra-doc/body
  "ci" 'clojure-toggle-ignore-surrounding-form
  "cq" 'cider-quit
  "cr" 'cider-hydra-repl/body
  "ct" 'cider-hydra-test/body
  "d"  'delete-other-windows
  "f" '(:ignore t :which-key "files")
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "fe" '(:ignore t :which-key "edit")
  "fed" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gd" 'magit-diff-unstaged
  "l" 'list-packages
  "o" '(:ignore t :which-key "org")
  "oa" 'org-agenda
  "ob" 'org-switchb
  "oc" 'org-capture
  "oe" 'org-export-dispatch
  "on" 'org-cycle-agenda-files
  "ol" 'org-insert-link
  "p" '(:ignore t :which-key "project")
  "pf" 'counsel-projectile-find-file
  "pp" 'counsel-projectile-switch-project
  "pt" 'treemacs-projectile
  "r" '(:ignore t :which-key "roam")
  "rb" 'org-roam-buffer-toggle
  "rc" 'org-roam-capture
  "rd" 'deft
  "rt" 'org-roam-dailies-goto-today
  "ri" 'org-roam-node-insert
  "rf" 'org-roam-node-find
  "t" '(:ignore t :which-key "toggles")
  "tg" 'golden-ratio-mode
  "tt" 'counsel-load-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))
 '(golden-ratio-mode t)
 '(package-selected-packages
   '(tagedit smex ido-completing-read+ clojure-mode-extra-font-locking deft nix-mode yasnippet-classic-snippets terraform-doc terraform-mode typescript-mode company rainbow-delimiters jenkinsfile-mode go-mode clj-refactor magit-todos evil-magit treemacs-magit cider-hydra lsp-ivy yaml-mode golden-ratio evil-org org-re-reveal counsel-projectile general treemacs-evil evil-collection evil undo-tree org-roam org-mode org-bullets crux treemacs-projectile treemacs exec-path-from-shell cider lsp-mode paredit clojure-mode magit projectile doom-themes doom-modeline counsel which-key diminish setup))
 '(safe-local-variable-values '((cider-clojure-cli-aliases . "dev")))
 '(undo-tree-auto-save-history nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
