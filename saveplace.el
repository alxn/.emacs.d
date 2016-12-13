;;; saveplace -- save location in file when saving files
;;; Commentary:
;;; Code:

(require 'saveplace)                   ;; get the package
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

(setq-default save-place t)            ;; activate it for all buffers

(setq save-place-file "~/.emacs.d/cache/places")

(require 'savehist)
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring);; ... my search entries
      savehist-autosave-interval 60    ;; save every minute (default: 5 min)
      savehist-file "~/.emacs.d/cache/history"
       ) ;; keep my home clean

(savehist-mode t)                ;; do customization before activation

(provide 'saveplace)
;;; saveplace.el ends here
