;; package -- Summary
;;; Commentary:
;;;  https://engdocs.uberinternal.com/go/additional_setup_pages/editors.html
;;; Code:

(setenv "USE_SYSTEM_GO" "1")
(setenv "GO111MODULE" "off")

(require 'lsp-mode)
(require 'go-mode)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  :hook '(go-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  ;; debug
  (lsp-log-io t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-print-performance t)
  (lsp-response-timeout 60)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 10000000)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
;  (lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
;  (lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))
;  (lsp-gopls-server-args '("-remote=auto" "-logfile=auto" "-debug=:6060" "-remote.debug=:0" "-rpc.trace"))
  (lsp-go-gopls-server-args '("-logfile=auto" "-debug=:0"))
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
                    :major-modes '(go-mode)
		    :language-id "go"
                    :remote? t
                    :server-id 'gopls-remote))
  )
(require 'tramp)
(add-to-list 'tramp-remote-path "/opt/go/path/bin")

;;				     (lambda () (cons lsp-go-gopls-server-path
;;						      lsp-go-gopls-server-args)))

; This is dangerous.
;  (lsp-document-sync-method 'full) ;; none, full, incremental, or nil

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
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
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; Provides snippet support.
;; https://andreacrotti.github.io/yasnippet-snippets/snippets.html
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook '(go-mode . yas-minor-mode))

;(add-hook 'go-mode-hook 'flycheck-mode)

;; Golang support - from Uber
;  (define-key esc-map "z" 'bazel-build)
(defun uber-go-mode-hook ()
  "Uber Golang Hook."
  (whitespace-mode -1) ; don't highlight hard tabs
  (setq
   tab-width 2         ; display tabs as two-spaces
   indent-tabs-mode 1  ; use hard tabs to indent
   fill-column 80)     ; set a reasonable fill width
  )

(add-hook 'go-mode-hook 'uber-go-mode-hook)

; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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
