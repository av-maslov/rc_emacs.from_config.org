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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                ;;;
;;; SETTINGS TO BE MOVED INTO MODULES ONCE STABLE: ;;;
;;;                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; START: Org mode

;; Auto-install without use-package
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; https://howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
  
  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.03))))  ;; 1.1
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05)))) ;; 1.25 
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.07))))  ;; .5
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.1)))) ;; 1.75
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
;; END: Org mode 
(use-package vterm
    :ensure t)
