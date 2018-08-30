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

(setenv "PATH" (concat (getenv "PATH") "/usr/local/bin:/Users/alun/gocode/bin"))
(setq exec-path (append exec-path '("/Users/alun/gocode/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'pallet)
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
(require 'compile)

;(defvar compilation-minor-mode-map
;(define-key map "\C-c\C-\" 'quit-compilation)

;; (defun quit-compilation ()
;;   "Quit the process made by the \\[compile] or \\[grep] commands."
;;   (interactive)
;;   (let ((buffer (compilation-find-buffer)))
;;     (if (get-buffer-process buffer)
;; 	(quit-process (get-buffer-process buffer))
;;       (error "The %s process is not running" (downcase mode-name)))))

(defun alun/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(setq compilation-window-height '25)
(setq compilation-scroll-output 'first-error)

(add-hook 'compilation-filter-hook
          'alun/colorize-compilation)

(add-hook 'Buffer-menu-mode-hook
	  'hl-line-mode)

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

(require 'company)
(require 'flycheck)

;(add-hook 'after-init-hook #'global-flycheck-mode)

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

;; org-mode
(require 'org)
(setq org-catch-invisible-edits 'show)
(setq org-startup-indented t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; Go Lang
(load-file "~/.emacs.d/go-setup.el")

; Should only be when in git, I guess.
(define-key vc-prefix-map "f" `vc-git-grep)

(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq indent-tabs-mode nil)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . web-mode))

;(require 'gnus-start)
;(setq gnus-init-file "~/.emacs.d/init-gnus.el")

(require 'bbdb)
(bbdb-initialize 'gnus 'message 'pgp)
(bbdb-mua-auto-update-init 'gnus 'message 'pgp)

(setq bbdb-message-all-addresses t ;; returns all mail addresses of a message.
      bbdb-complete-mail-allow-cycling t ;; cycle mail addresses completing.
      bbdb-update-records-p t ;; Search for existing, create if new.
      bbdb-pop-up-layout 'one-line
      bbdb-pop-up-window-size 0.1
      bbdb-ignore-redundant-mails t)

(defun alun-bbdb-quiet-save (record)
  "Alun's quiet bbdb save for the new RECORD."
  (bbdb-save nil t))

(add-hook 'bbdb-create-hook 'alun-bbdb-quiet-save)

(setq bbdb-auto-notes-rules
      (list
       '("Subject"    (".*" subjects 0))
       '("User-Agent" (".*" mailer identity nil))
       '("Date"       (".*" lastseen identity nil))
       '("X-Mailer"      (".*" mailer 0))))

; To disable automatic notes collection for some messages:
(setq bbdb-auto-notes-ignore-headers
      '((("Organization" . "^Gatewayed from\\|^Source only"))))

(add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)

;; TODO:
;; https://github.com/vincentbernat/dot.emacs/blob/cb2472ac4e7b8baac2e4499cf438e955430e9626/bbdb.conf.el
;;   "Canonicalize SUBJECT."

;; (defun alun-bbdb-trim-subjects (record)
;;   "Remove all but the first 5 lines from the subjects in the notes field of a BBDB (RECORD)."
;;   (let* ((sep (get 'subjects 'field-separator))
;;          (foo (reverse
;;                (split-string
;;                 (or (bbdb-record-getprop record 'subjects) "")
;;                 sep)))
;;          (num-to-keep 5)
;;          (new-subj ""))
;;     (while (and (> num-to-keep 0) (> (length foo) 0))
;;       (if (> (length (car foo)) 0)
;;           (setq new-subj (concat (car foo)
;;                                  (if (> (length new-subj) 0)
;;                                      (concat sep new-subj)
;;                                    ""))
;;                 num-to-keep (- num-to-keep 1)))
;;       (setq foo (cdr foo)))
;;     (bbdb-record-putprop record 'subjects new-subj)))
;; (add-hook 'bbdb-change-hook 'alun-bbdb-trim-subjects)

;;; http://bbdb.sourceforge.net/bbdb.html#SEC61

;;; If the variable bbdb/mail-auto-create-p is set to the symbol
;;; bbdb-ignore-most-messages-hook, then the variable
;;; bbdb-ignore-most-messages-alist will determine which messages
;;; should have records automatically created for them. The format of
;;; this alist is

;;; Alist describing which messages not to automatically create BBDB records for.
(setq bbdb-ignore-message-alist
      '(("From" . "github")
	("To" . "github")
	("From" . "@docs.google.com")
	("From" . "drive-shares-noreply@google.com")
	("From" . "phabricator@uberatc.com")
	("From" . "phab@code.uberinternal.com")))

(global-magit-file-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(org-agenda-files (quote ("/Users/alun/Documents/Org/")))
 '(package-selected-packages
   (quote
    (flymake-cppcheck flymake-google-cpplint modern-cpp-font-lock flymake-puppet puppet-mode flycheck-yamllint yaml-mode visual-fill-column company-irony company-irony-c-headers flycheck-irony irony auto-complete-clang clang-format flycheck-clang-analyzer flycheck-clang-tidy groovy-imports groovy-mode ac-emoji company-emoji emojify protobuf-mode thrift go-direx project-explorer web-mode cov editorconfig sr-speedbar all-the-icons all-the-icons-dired neotree minimap phabricator counsel-bbdb bbdb helm-bbdb helm-company helm-cscope helm-flycheck helm-flymake helm-flyspell helm-git helm-git-files helm-git-grep helm-ispell xcscope hyde json-mode plantuml-mode cask chef-mode company electric-spacing emoji-cheat-sheet-plus epl gh gist git git-commit git-messenger gitattributes-mode gitconfig gitconfig-mode magit magit-popup vagrant markdown-mode benchmark-init boxquote confluence xml-rpc go-dlv go-eldoc flycheck-plantuml flycheck go-autocomplete go-complete go-errcheck go-gopath go-guru go-impl go-mode go-rename go-scratch golint pallet)))
 '(send-mail-function (quote smtpmail-send-it))
 '(sgml-basic-offset 2)
 '(url-cookie-file "~/.emacs.d/cache/cookies")
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
