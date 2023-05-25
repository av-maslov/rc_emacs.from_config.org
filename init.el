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

(load "~/.emacs.d/user/ivy.el")  ;; swiper/counsel
(load "~/.emacs.d/user/colors.el")
(load "~/.emacs.d/user/smex.el")
(load "~/.emacs.d/user/acejump.el")
(load "~/.emacs.d/user/highlight-symbol.el")
(load "~/.emacs.d/user/sly.el") ;; sbcl
(load "~/.emacs.d/user/yasnippet-and-autocomlete.el")
(load "~/.emacs.d/user/paredit.el")
(load "~/.emacs.d/user/magit_set_up.el")

;; START: Python 
;; https://fredrikmeyer.net/2020/08/26/emacs-python-venv.html
;; pyvenv-activate
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t))

;;(use-package elpy
;;  :ensure t
;;  :init
;;  (elpy-enable))

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))
;; END: PYTHON

;; START: rg and fzf
(use-package rg
  :config
  (rg-enable-default-bindings) ;;  C-c s r (rg)
  )

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))
;; END: rg and fzf
