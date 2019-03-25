;;; Package -- Summary
;;; Commentary:
;;; Code:

(require 'gnus)
(require 'gnus-cite)
(require 'smtpmail)
(require 'mml)
(require 'eudc)
(require 'nnir)

;(require 'spotlight)
;(require 'gnus-spotlight)
;(gnus-spotlight-insinuate)

(setq user-mail-address "alun@uber.com"
      user-full-name "Alun Evans")

(setq gnus-select-method
      '(nnimap "Uber"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-list-pattern ("INBOX" "*"))
	       (nnimap-expunge-on-close never)
	       (nnimap-stream ssl)
	       (nnir-search-engine imap)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defvar gnus-permanently-visible-groups)
(setq gnus-permanently-visible-groups ".*.*")

(defvar gnus-gcc-mark-as-read)
(setq gnus-gcc-mark-as-read t)
(setq gnus-message-archive-group "nnimap+Uber:[Gmail]/Sent Mail")

; Asks anoying questions all the time.
(setq gnus-novice-user nil)

;; Turn on GPG
(setq mml-secure-openpgp-signers "219ED135")
;; We want to be able to read the emails we wrote.
(setq mml-secure-openpgp-encrypt-to-self t)

; Hide HTML mail.
;;(require 'mm-decode)

(setq mm-discouraged-alternatives
      '("text/html" "text/richtext")
      mm-automatic-display
      (remove "text/html" mm-automatic-display))

; Hide quoted text.
(setq gnus-treat-hide-citation t)

; What is a large group.
(setq gnus-large-newsgroup 1000)

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-message-sign)
;(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Fix the replying.

(defun alun-date-time (messy-date)
  "Return a date time string I like from the given MESSY-DATE."
  (condition-case ()
      (format-time-string "On %a %d %b '%g at %H:%M" (safe-date-to-time messy-date))
    (error "  -   ")))

(defun alun-message-citation-line-function ()
  "Alun's custom citation line."
  (when message-reply-headers
    (insert "\n\n")
    (insert (alun-date-time (mail-header-date message-reply-headers)))
    (insert " " (mail-header-from message-reply-headers) " wrote:\n")
    (insert message-yank-prefix "\n")))

;     (insert "\n" message-yank-prefix)
;     (insert "On " (mail-header-date message-reply-headers) " ")
;     (insert (mail-header-from message-reply-headers) "wrote about ")
;     (insert (mail-header-subject message-reply-headers) " :\n")
;     (insert message-yank-prefix "\n")
;    ))

(setq message-citation-line-function
      'alun-message-citation-line-function)

(setq message-cite-function
      'message-cite-original-without-signature)

;
; When we load a summary buffer split the window
; horizontally [25% group, rest summary]
;
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 0.25 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))


;
; When we load a article buffer split the window
; horizontally [25% group, rest summary/article]
;                               summary 25%
;                               article rest
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 0.25 (group 1.0))
               (vertical 1.0
                         (summary 0.25 point)
                         (article 1.0)))))

;
; See gnus-win.el to work out what is going on here...
;

(gnus-add-configuration
 '(reply
   (if (buffer-live-p gnus-summary-buffer)
       '(horizontal 1.0
		   (vertical 0.25 (group 1.0))
		   (vertical 1.0
			     (summary 0.25 point)
			     (message 1.0))))))

(gnus-add-configuration
 '(reply-yank
   (if (buffer-live-p gnus-summary-buffer)
       '(horizontal 1.0
		   (vertical 0.25 (group 1.0))
		   (vertical 1.0
			     (summary 0.25 point)
			     (message 1.0))))))

(gnus-add-configuration
 '(forward
   (if (buffer-live-p gnus-summary-buffer)
       '(horizontal 1.0
		   (vertical 0.25 (group 1.0))
		   (vertical 1.0
			     (summary 0.25 point)
			     (message 1.0))))))

; When we go to compose an email, if the summary buffer exists,
; put the new message where the article buffer would go.
; otherwise we'll just fill the full window.
;
(gnus-add-configuration
 '(message
   (if (buffer-live-p gnus-summary-buffer)
       '(horizontal 1.0
		   (vertical 0.25 (group 1.0))
		   (vertical 1.0
			     (summary 0.25 point)
			     (message 1.0))))))

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;;; Its value is ((email) (firstname) (firstname name))
(setq eudc-inline-query-format '((email)
                                 (aka)
                                 (firstname)
                                 (firtname name)
                                 (lastname)
                                 (name)))

(eval-after-load "message"
  '(define-key message-mode-map [(control ?c) (tab)] 'eudc-expand-inline))

(eval-after-load "mail"
 '(define-key mail-mode-map [(control ?c) (tab)] 'eudc-expand-inline))

(setq gnus-parameters
      '(("^INBOX$"
	 (display . all)
	 )
	("^Commander$"
	 (gnus-summary-hide-all-threads)
	 )
	)
      )

;;; Gnus Gmail search.
(add-to-list 'nnir-imap-search-arguments '("Uber" . "X-GM-RAW"))
(setq nnir-imap-default-search-key "Uber")


;;; %U%R%z%I%(%[%4L: %-23,23f%]%) %B

;;; gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
;;; gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))

(setq-default
 gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%] %B%s%)\n"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date))

;; eye-candy for the summary view
(when window-system
  (setq gnus-sum-thread-tree-indent "")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

;;; .gnus ends here
