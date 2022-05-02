;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'ispell)

(cond
   ((string-equal system-type "darwin") ; Mac OS X
    (progn (message "Mac OS X Spelling"))
    (add-to-list 'exec-path "/usr/local/Cellar/aspell/0.60.8/bin")
    (setq ispell-program-name "aspell"
          ispell-local-dictionary "british"
          ispell-dictionary "british"
          ispell-dictionary-alist
          (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                           ("-B" "-d" "english" "--dict-dir"
                            "/usr/local/Cellar/aspell/0.60.8/lib/aspell-0.60")
                           nil iso-8859-1)))
            `((nil ,@default)
              ("british" ,@default))))
    )
)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'c-mode-common-hook
	  'flyspell-prog-mode)

(provide 'spell)
;;; spell.el ends here
