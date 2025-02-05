(use-package general
  :bind (("s-}" . evil-window-next)
         ("s-{" . evil-window-prev)
         ("s-w" . delete-window))
  :config
  (setq general-evil-setup (boundp 'evil-statez))
  (general-create-definer zt/leader-key-def
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-M-SPC"))

(defun navigate-to-daily-journal ()
  "Navigate to today's org file in ~/OrgFiles/Daily/yyyy-mm-dd.org.
   Create the file if it doesn't exist."
  (interactive)
  (let* ((daily-dir "~/OrgFiles/Daily")
         (today (format-time-string "%Y-%m-%d"))
         (filename (concat today ".org"))
         (filepath (expand-file-name filename daily-dir)))
    
    ;; Create the directory if it doesn't exist
    (unless (file-exists-p daily-dir)
      (make-directory daily-dir t))
    
    ;; Create the file if it doesn't exist
    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert (concat "#+TITLE: Daily Journal - " today "\n\n"))))
    
    ;; Open the file
    (find-file filepath)))

(zt/leader-key-def
  "SPC" 'counsel-M-x
  "TAB" 'crux-switch-to-previous-buffer
  "/" 'counsel-projectile-rg
  ";" 'org-open-at-point
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "a" 'avy-goto-word-or-subword-1
  "b" '(:ignore t :which-key "buffers")
  "bb" 'counsel-switch-buffer
  "bk" 'kill-buffer
  "c" '(:ignore t :which-key "clojure")
  "cc" 'cider-connect-clj
  "cj" '(:ignore t :which-key "jack in")
  "cjc" 'cider-jack-in-clj
  "cjs" 'cider-jack-in-cljs
  "cg" 'lsp-find-definition
  "cu" 'lsp-find-references
  "cb" 'cider-eval-buffer
  "cd" 'cider-hydra-doc/body
  "ce" 'cider-hydra-eval/body
  "cf" 'cider-format-buffer
  "ck" 'cider-repl-clear-buffer
  "ci" 'clojure-toggle-ignore-surrounding-form
  "cn" 'cider-test-run-ns-tests
  "cq" 'cider-quit
  "cr" 'cider-hydra-repl/body
  "cs" 'sfm-toggle
  "ct" 'cider-hydra-test/body
  "d"  'delete-other-windows
  "e"  'ace-window
  "f" '(:ignore t :which-key "files")
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "fe" '(:ignore t :which-key "edit")
  "fed" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
  "g" '(:ignore t :which-key "git")
  "gb"  'magit-blame-addition
  "gc"  'magit-branch-or-checkout
  "gd"  'magit-diff-unstaged
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gl"  '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gr"  'magit-rebase
  "gs"  'magit-status
  "j" '(:ignore t :which-key "jet")
  "jj" 'jet
  "jc" 'jet-json-to-clipboard
  "k"  'navigate-to-daily-journal ;;'(lambda () (interactive) (find-file (expand-file-name "~/OrgFiles/Journal.org")))
  "l" 'package-list-packages
  "o" '(:ignore t :which-key "org")
  "oa" 'org-agenda
  "ob" 'org-switchb
  "oc" 'org-capture
  "od" 'org-deadline
  "oe" 'org-export-dispatch
  "on" 'org-cycle-agenda-files
  "ol" 'org-insert-link
  "os" 'org-schedule
  "ot" 'org-todo
  "p" '(:ignore t :which-key "project")
  "pf" 'counsel-projectile-find-file
  "pp" 'counsel-projectile-switch-project
  "pt" 'projectile-run-vterm-other-window
  "r" '(:ignore t :which-key "roam")
  "rb" 'org-roam-buffer-toggle
  "rc" 'org-roam-capture
  "rd" 'deft
  "rt" 'org-roam-dailies-goto-today
  "ri" 'org-roam-node-insert
  "rf" 'org-roam-node-find
  "t" '(:ignore t :which-key "toggles")
  "tg" 'golden-ratio-mode
  "tt" 'counsel-load-theme
  "u"  'universal-argument
  "w" '(:ignore t :which-key "windows")
  "wd" 'delete-window
  "wk" 'evil-window-delete
  "wo" 'delete-other-windows)
