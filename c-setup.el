

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

(defconst uber-cpp-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open . 0)
     (substatement-label . 0)
     (label . 0)
     (statement-cont . +)))
  "Uber C++ Style from stroustrup")

(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++14")))

(add-hook 'c++-mode-hook
          (lambda ()
	    (c-add-style "uber" uber-cpp-style t)
	  ))

;(clang-format-buffer &optional STYLE ASSUME-FILE-NAME)

(add-hook 'c++-mode-hook
          (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))

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
            ;; Add uber style
            (c-add-style
             "uber" uber-c-style)))

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

;;; https://emacs.stackexchange.com/questions/38822/cscope-minor-mode-not-enabled-for-c-in-emacs-25
(cscope-setup)
