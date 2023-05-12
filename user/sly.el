(add-to-list 'load-path "~/.emacs.d/plugins/sly")
(require 'sly-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

