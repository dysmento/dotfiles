(use-package projectile
  :bind (("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

(use-package counsel-projectile)
