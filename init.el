(setq path-to-emacsd "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/user/")
(load "basics.el")
(load "~/.emacs.d/user/melpa-and-auto-install.el")

(add-to-list 'load-path (get-full-path "plugins/"))
(add-to-list 'load-path (get-full-path "plugins/smex/"))
(add-to-list 'load-path (get-full-path "plugins/aceJump/"))
(add-to-list 'load-path (get-full-path "plugins/highlight-symbol/"))
(add-to-list 'load-path (get-full-path "plugins/yasnippet/"))
;;(add-to-list 'load-path (get-full-path "plugins/clojure-mode/"))
(add-to-list 'load-path (get-full-path "scripts/"))
(load "move-line.el")
(load "colors.el")
(load "navigation.el")
(load "smex.el")
(load "acejump.el")
(load "highlight-symbol.el")
;;(load "emacs-ess.el")

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))

(require 'epa-file)
(epa-file-enable)

;;(load "~/.emacs.d/user/whitespace.el")
(load "~/.emacs.d/user/yasnippet-and-autocomlete.el")

;; Magit
(global-set-key (kbd "C-x G") 'magit-status)

;;(load "lisp-settings.el")
;;(load "paredit-settings.el")
;;(load "rainbow-delimiters-settings.el")


;; AUTO FILL MODE
;; https://www.emacswiki.org/emacs/AutoFillMode
;; Type M-x auto-fill-mode to activate the MinorMode for the current buffer, or put the following in your .emacs to activate it for all text mode buffers:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(auto-fill-mode t)
;; END AUTO FILL MODE

;;(load "activate-markdown-mode.el")

;;
;; sly 
;;
(add-to-list 'load-path "~/.emacs.d/plugins/sly")
(require 'sly-autoloads)
;;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/sbcl")
(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

;;

(global-set-key (kbd "<delete>") 'delete-region)


;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
