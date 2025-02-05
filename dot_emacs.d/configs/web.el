(use-package tagedit
  :hook web-mode
  :bind (("C-)" . tagedit-forward-slurp-tag)
         ("C-}" . tagedit-forward-barf-tag)))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.php\\'" "\\.js\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package emmet-mode)

(use-package js
  :hook subword-mode
  :bind ("M-s-l" . json-pretty-print-buffer))

(setq js-indent-level 2)
