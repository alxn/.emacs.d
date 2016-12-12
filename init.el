;;; package --- Summary:
;;; Commentary:
;; Save the starting time...
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;; Code:

;; Setup the packaging system.
(package-initialize)

(setq load-path
      (append
       (list "~/.emacs.d/elisp/"
	     "/usr/local/share/emacs/site-lisp/"
	     "~/.local/share/emacs/site-lisp/")
       load-path))

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require `pallet)
(pallet-mode t)

;; See where we spend out time.
;; Use M-x benchmark-init/show-durations-<TAB>
;(require 'benchmark-init)
;(benchmark-init/activate)

;; no obnoxious tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(set-background-color "black")
(set-foreground-color "white")

;;; Nice size for the default window
(defun get-default-height ()
  "Get the default hight for the window."
  (/ (- (display-pixel-height) 120)
     (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

(require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Code styles
(load-file "~/.emacs.d/c-setup.el")

;; C styles
(load-file "~/.emacs.d/v-setup.el")

(cond
   ((string-equal system-type "darwin") ; Mac OS X
    (progn (message "Mac OS X"))
    (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
    (setq mac-option-modifier nil) ;; Default was meta
    (setq mac-command-modifier 'meta)
    )
)

;; SGML styles
(load-file "~/.emacs.d/sgml-setup.el")

;; From HomeBrew.
(require 'xcscope)
(setq cscope-do-not-update-database t)

(require 'company)
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'json-mode)
(require 'markdown-mode)

(require 'electric-spacing)
(require 'git-messenger)
(setq git-messenger:show-detail t)

(require 'helm)
(require 'emoji-cheat-sheet-plus)
(require 'gh)
(require 'gist)
;(require 'git-modes)

;; End From HomeBrew.

;; backups
(load-file "~/.emacs.d/backups.el")

;; saveplace
(load-file "~/.emacs.d/saveplace.el")

;; recentf
(load-file "~/.emacs.d/recentf.el")

;; doxymacs
(load-file "~/.emacs.d/doxymacs.el")

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-key esc-map "*" 'revert-buffer)
(define-key esc-map "z" 'compile)

(line-number-mode t)
(column-number-mode t)
(display-time-mode t)

(setq frame-title-format `(,(user-login-name) "@" ,(system-name) "  [%b]" ))

; http://superuser.com/questions/277956/emacs-variable-to-open-with-in-original-frame
(setq ns-pop-up-frames nil)


(setenv "DICTIONARY" "en_GB")
;; Spelling
(load-file "~/.emacs.d/spell.el")

;; set up a bunch of auto-mode-alist stuff
(load-file "~/.emacs.d/auto-mode-alist-setup.el")

(add-to-list 'load-path "~/.emacs.d/hyde/")
(require 'hyde)

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; Go Lang
(load-file "~/.emacs.d/go-setup.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(package-selected-packages
   (quote
    (helm-bbdb helm-company helm-cscope helm-flycheck helm-flymake helm-flyspell helm-git helm-git-files helm-git-grep helm-ispell helm-package xcscope hyde json-mode plantuml-mode cask chef-mode company electric-spacing emoji-cheat-sheet-plus epl gh gist git git-commit git-messenger gitattributes-mode gitconfig gitconfig-mode magit magit-popup vagrant markdown-mode benchmark-init boxquote confluence xml-rpc go-dlv go-eldoc flycheck-plantuml flycheck go-autocomplete go-complete go-errcheck go-gopath go-guru go-impl go-mode go-rename go-scratch golint pallet)))
 '(send-mail-function (quote smtpmail-send-it))
 '(sgml-basic-offset 2)
 '(user-full-name "Alun Evans")
 '(user-mail-address "alun@uber.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
