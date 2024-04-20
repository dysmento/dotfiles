(setq use-short-answers t)
(setq inhibit-startup-screen t)
(setq save-place t)
(setq fill-column 80)

(setq backup-directory-alist '(("." . "~/.emacs.backups/")))

(use-package no-littering
  :config (setq auto-save-file-name-transforms
                `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package crux
  :bind (("s-r" . crux-recentf-find-file)
	 ("C-c t" . crux-visit-term-buffer)
	 ("s-<backspace>" . crux-kill-whole-line)
	 ("s-d" . crux-duplicate-current-line-or-region)))

(setq auto-save-default nil)
(server-start)
