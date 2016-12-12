

;; c.f. http://inst.eecs.berkeley.edu/~cs47b/emacs/lisp/cc-mode/cc-styles.el

(defconst uber-c-style
  '((c-basic-offset . 4)
    (c-auto-newline)
    (comment-multi-line t)
    (c-offsets-alist
     (knr-argdecl-intro . +)
     (knr-argdecl . 0)
     (statement-cont . +)
     (statement-case-open . +)
     (substatement-open . 0)
     (label . 0)
     (case-label . 0)))
  "Uber C Style for CC-MODE")

;;
;; LwIP Style based on:
;; http://lwip.wikia.com/wiki/Contributing_to_lwIP
;;   Source code style
;;
;; do not use tabs.
;; indentation is two spaces per level (i.e. per tab).
;; end debug messages with a trailing newline (\n).
;; one space between keyword and opening bracket.
;; no space between function and opening bracket.
;; one space and no newline before opening curly braces of a block.
;; closing curly brace on a single line.
;; spaces surrounding assignment and comparisons.
;; use current source code style as further reference.
(setq load-path
      (append
       (list "~/.emacs.d/elisp/"
	     "/usr/local/share/emacs/site-lisp/"
	     "~/.local/share/emacs/site-lisp/")
       load-path))

;;
(defconst lwip-c-style
  '("lwip"
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "LwIP C Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add Uber style
            (c-add-style
             "UBER" uber-c-style)))

;;
;; Linux style as required in Documentation/CodingStyle
;;
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-c-style
  '((c-basic-offset . 8)
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "Linux C Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only" linux-c-style)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; Add kernel style
;;             (c-add-style
;;              "linux-tabs-only"
;;              '("linux" (c-offsets-alist
;;                         (arglist-cont-nonempty
;;                          c-lineup-gcc-asm-reg
;;                          c-lineup-arglist-tabs-only))))))


;; (file-exists-p (concat
;; 		(file-name-directory filename)
;; 		"Kbuild"))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (if (and filename
                         (or 
			  (string-match "/kernel" filename)
			  (locate-dominating-file filename "Kbuild")))
		(progn
		  (message "Kernel mode: %s" filename)
		  (c-set-style "linux-tabs-only"))
		(progn
		  (message "Xockets mode: %s" filename)
		  (c-set-style "XOCKETS")
		  (setq indent-tabs-mode nil))))))

