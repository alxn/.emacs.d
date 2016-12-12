;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'go-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'before-save-hook 'gofmt-before-save)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(require 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(require 'go-guru)

;; Inside a buffer of Go source code, select an expression of
;; interest, and type `C-c C-o d' (for "describe") or run one of the
;; other go-guru-xxx commands.  If you use `menu-bar-mode', these
;; commands are available from the Guru menu.

(go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

(provide 'go-setup)
;;; go-setup.el ends here
