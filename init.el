(load "~/.emacs.d/user/basics.el")

(add-to-list 'load-path "~/.emacs.d/user/")
(add-to-list 'load-path (get-full-path "plugins/"))
(add-to-list 'load-path (get-full-path "plugins/smex/"))
(add-to-list 'load-path (get-full-path "plugins/aceJump/"))
(add-to-list 'load-path (get-full-path "plugins/highlight-symbol/"))
(add-to-list 'load-path (get-full-path "plugins/yasnippet/"))
(add-to-list 'load-path (get-full-path "scripts/"))
(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(load "colors.el")
(load "smex.el")
(load "acejump.el")
(load "highlight-symbol.el")
(load "sly.el")
(load "~/.emacs.d/user/yasnippet-and-autocomlete.el")

;; in basics;
;;(require 'use-package)
;;(with-eval-after-load 'info
;;  (info-initialize)
;;  (add-to-list 'Info-directory-list
;;               "~/.emacs.d/site-lisp/use-package/"))
;;(require 'epa-file)
;;(epa-file-enable)

;;(load "emacs-ess.el")
;;(load "lisp-settings.el")
;;(load "paredit-settings.el")
;;(load "rainbow-delimiters-settings.el")
;;(load "~/.emacs.d/user/whitespace.el")
;;(load "activate-markdown-mode.el")

