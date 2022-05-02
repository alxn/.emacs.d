(add-to-list 'auto-mode-alist '("/Kconfig$" . kconfig-mode))
(add-to-list 'auto-mode-alist '("/Kconfig\\..*$" . kconfig-mode))
(add-to-list 'auto-mode-alist '("_defconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("/Kbuild$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.star$" . bazel-build-mode))
(add-to-list 'auto-mode-alist '("\\.bazel$" . bazel-build-mode))

;; (add-to-list 'auto-mode-alist
;;              '("/\\(rfc\\|std\\)[0-9]+\\.txt\\'" . rfcview-mode))

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . espresso-mode))

;; (add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)" . markdown-mode))

(add-to-list 'auto-mode-alist '("/git-rebase-todo$" . conf-mode))

(add-to-list 'auto-mode-alist '("/.gitconfig". conf-mode))

(add-to-list 'auto-mode-alist '("/.*patch$" . diff-mode))

(add-to-list 'auto-mode-alist '("\\.sdl$" . javascript-mode))

(add-to-list 'auto-mode-alist '("^Vagrantfile" . ruby-mode))
