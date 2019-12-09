;;; package -- Summary
;;; Commentary:
;;;  https://engdocs.uberinternal.com/go/additional_setup_pages/editors.html
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(require 'lsp-mode)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(require 'go-mode)
;(add-hook 'go-mode-hook 'flycheck-mode)
;(require 'go-autocomplete)

;(require 'auto-complete-config)
;(ac-config-default)

;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;go-guru-definition
;(define-key go-mode-map (kbd "M-.") 'godef-jump)
;(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

;(with-eval-after-load 'go-mode
;  (require 'go-autocomplete))

;(require 'go-complete)
;(add-hook 'completion-at-point-functions 'go-complete-at-point)

;(require 'go-eldoc)
;(add-hook 'go-mode-hook 'go-eldoc-setup)

;(require 'go-guru)

;; Inside a buffer of Go source code, select an expression of
;; interest, and type `C-c C-o d' (for "describe") or run one of the
;; other go-guru-xxx commands.  If you use `menu-bar-mode', these
;; commands are available from the Guru menu.

;(go-guru-hl-identifier-mode)
;(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; Golang support - from Uber
(defun uber-go-mode-hook ()
  "Uber Golang Hook."
  (whitespace-mode -1) ; don't highlight hard tabs
  (auto-complete-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq
   gofmt-command "/Users/alun/gocode/bin/goimports"
   tab-width 2         ; display tabs as two-spaces
   indent-tabs-mode 1  ; use hard tabs to indent
   fill-column 100)    ; set a reasonable fill width
  )

(add-hook 'go-mode-hook 'uber-go-mode-hook)

;; Spell check.
(add-hook 'go-mode-hook 'flyspell-prog-mode)

;; Enable the project explorer side bar (don't forget about minimap-mode)
;(add-hook 'go-mode-hook 'project-explorer-open)

;; Make it readable.
;(add-hook 'project-explorer-mode-hook 'hl-line-mode)

;; also c.f. neotree-mode with neo-smart-open.
;;(add-hook 'gnus-group-mode-hook 'hl-line-mode)
;;https://www.emacswiki.org/emacs/SrSpeedbar
;; http://cedet.sourceforge.net/speedbar.shtml
;; lots of MODES.
;; also neotree
;; https://github.com/jaypei/emacs-neotree

;; Need it for switching branches.
(add-hook 'go-mode-hook 'global-auto-revert-mode)

(provide 'go-setup)
;;; go-setup.el ends here
