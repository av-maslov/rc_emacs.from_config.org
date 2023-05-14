(load "~/.emacs.d/user/basics.el")

(setq path-to-emacsd "~/.emacs.d/")
(defun get-full-path (subpath)
  (concat path-to-emacsd subpath))


(add-to-list 'load-path "~/.emacs.d/user/")
(add-to-list 'load-path "~/.emacs.d/plugins/swiper")
(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/smex/")
(add-to-list 'load-path "~/.emacs.d/plugins/aceJump/")
(add-to-list 'load-path "~/.emacs.d/plugins/highlight-symbol/")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet/")
(add-to-list 'load-path "~/.emacs.d/scripts/")

(load "~/.emacs.d/user/ivy.el")
(load "~/.emacs.d/user/colors.el")
(load "smex.el")
(load "acejump.el")
(load "highlight-symbol.el")
(load "sly.el")
(load "~/.emacs.d/user/yasnippet-and-autocomlete.el")

