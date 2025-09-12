(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'auto-fill-mode)
  (add-hook 'markdown-mode-hook (lambda () (display-fill-column-indicator-mode t)
                                           (set-fill-column 100))))

(use-package adoc-mode)

