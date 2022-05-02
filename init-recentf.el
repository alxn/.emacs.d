;; recentf
(require 'recentf)    ;; save recently used files
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ;; access it.
(recentf-mode t)                  ;; turn it on
