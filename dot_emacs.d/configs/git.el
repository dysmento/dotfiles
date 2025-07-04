(use-package magit
  :bind (("C-M-;" . magit-status))
  :custom
  (magit-git-executable
    (string-trim (shell-command-to-string "which git"))))
