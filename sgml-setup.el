
;; Xml Setup

;; zencoding renamed to emmet: http://emmet.io/

(require 'nxml-mode)
;;(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes

(add-hook 'nxml-mode-hook
          (lambda ()
	    (setq nxml-outline-child-indent 2)
            (setq indent-tabs-mode nil)
	    )
	  )

(add-hook 'sgml-mode-hook
          (lambda ()
	    (setq sgml-basic-offset 2)
            (setq indent-tabs-mode nil)
	    )
	  )

