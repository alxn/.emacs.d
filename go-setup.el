;;; package -- Summary
;;; Commentary:
;;;  https://engdocs.uberinternal.com/go/additional_setup_pages/editors.html
;;; Code:

(setenv "USE_SYSTEM_GO" "1")
(setenv "GO111MODULE" "off")

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :custom
  ;; debug
;  (lsp-print-io t)
;  (lsp-trace t)
;  (lsp-print-performance t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 60)
  (lsp-prefer-flymake t) ;; t(flymake), nil(lsp-ui), or :none
  (lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))
;  (lsp-gopls-server-args '("-rpc.trace" "-logfile" "/tmp/gopls.log"))
  )


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-code-actions t)
)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; Provides snippet support.
;; https://andreacrotti.github.io/yasnippet-snippets/snippets.html
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
  (setq
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
