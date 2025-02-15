(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("C-s" . swiper)
	 ("s-f" . swiper)
	 ("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x))
  :config
  (add-to-list 'package-selected-packages 'counsel)
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-count-format "%d/%d "))

(use-package ivy-rich
  :config
  (ivy-rich-mode))

(use-package ivy-prescient
  :config
  (setq prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (load-theme 'doom-zenburn t))

(use-package winum
  :config
  (winum-mode))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

(set-face-attribute 'default nil :font "Fira Code" :height 150)

(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 154))
