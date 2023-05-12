(setq-default fill-column 65)

;; org mode: no empty line 
;; C-h v org-blank-before-new-entry 
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq inhibit-startup-message t) ;; No splash screen
;;(setq initial-scratch-message nil) ;; No scratch message

;;;; http://ergoemacs.org/emacs/emacs_toggle-word-wrap.html
(setq tooggle-word-wrap t)

(setq path-to-emacsd "~/.emacs.d/")
(defun get-full-path (subpath)
  (concat path-to-emacsd subpath))

(setq backup-directory-alist
      `((".*" . , "~/emacstemp/")))

(setq auto-save-file-name-transforms
      `((".*" , "~/emacstemp/" t)))

;; ---------------------- AUTO INSTALL PACKAGES (If needed)
(add-to-list 'load-path (get-full-path "~/.emacs.d/"))
(load "~/.emacs.d/auto-install.el")
;; ---------------------- End AUTO INSTALL PACKAGES (If needed)

;; Install https://github.com/jwiegley/use-package 

;; (dolist (package '(use-package))
;;    (unless (package-installed-p company)
;;        (package-install company)))

;;; After symon is installed
;; (require 'symon)
;; (add-to-list 'load-path path-to-emacsd)
(add-to-list 'load-path (get-full-path "settings/"))
(add-to-list 'load-path (get-full-path "settings/plugins-settings/"))
(add-to-list 'load-path (get-full-path "customizations/"))
(add-to-list 'load-path (get-full-path "plugins/"))
(add-to-list 'load-path (get-full-path "plugins/smex/"))
(add-to-list 'load-path (get-full-path "plugins/aceJump/"))
(add-to-list 'load-path (get-full-path "plugins/highlight-symbol/"))
(add-to-list 'load-path (get-full-path "plugins/yasnippet/"))
(add-to-list 'load-path (get-full-path "plugins/clojure-mode/"))
;; scripts
(add-to-list 'load-path (get-full-path "scripts/"))
(load "move-line.el")
;;... Settings
;; (load "settings.el") --no such file
(load "colors.el")
(load "focus.el")
(load "ui.el")
(load "navigation.el")
;;... Packages settings
(load "smex.el")
(load "acejump.el")
(load "highlight-symbol.el")
;; (load "ocaml-settings.el")
;;
;; Caused Warning (bytecomp): reference to free variable ‘helm-alive-p’ 
;; (require 'sr-speedbar) ;; sr-speedbar-open/close
;;
;; Load following files if these packages have already been installed using package manager
;; M-x package-list-packages
;; (load "magit.el")
;; (load "scalamode.el")

;;(load "emacs-ess.el")
;;(load "emacs-ess-juliamode.el") ;; Maybe should be before ess-mode.el

;;-------------------- PACKAGE DIRECTORIES MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))



;;-------------------- END PACKAGE DIRECTORIES MELPA

;;------------------- Cider (Clojure)
;;(load "clojure-settings.el")
;;------------------- End Cider (Clojure)


;;------------------- HASKELL MODE
;;(require 'cl-lib)
;;(load "haskellmode.el")
;;------------------- END HASKELL MODE
;;-------------------- Vim mode 
;; (require 'evil)
;; (evil-mode 1)
;;-------------------- End Vim mode 
;;-------------------- EPA
(require 'epa-file)
(epa-file-enable)
;;-------------------- End EPA
;;-------------------- WHITESPACE -----------------------;;
(require 'whitespace)
;; highlight long lines
;; http://www.emacswiki.org/emacs/HighlightLongLines
(setq whitespace-style '(lines))
(setq whitespace-line-column 80)
(global-whitespace-mode 1)
(setq whitespace-style '(tabs trailing lines tab-mark))
;; end highlight long lines
;;-------------------- END WHITESPACE --------------------;;

;;-------------------- YASNIPPET AND AUTOCOMPLETE ---------------------;;
;; ... Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;; http://sethlakowske.com/why-i-use-emacs/fix-yasnippet-and-autocomplete-tab-key-collision/
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
;; ... End Yasnippt - separate file doesn't work

;; Auto-complete
;; after auto-complete is installed
;; http://auto-complete.org/doc/manual.html#after-installation-check
;; http://auto-complete.org/doc/manual.html#installation
;; https://github.com/auto-complete/auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)
(global-auto-complete-mode t)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; End Auto-complete
;;-------------------- END YASNIPPET AND AUTOCOMPLETE ---------------------;;

;; Magit
(global-set-key (kbd "C-x G") 'magit-status)

;;(load "lisp-settings.el")
(load "paredit-settings.el")
(load "rainbow-delimiters-settings.el")

(defun ap ()
  (interactive)
  (auto-complete-mode t)
  (paredit-mode t))

;; AUTO FILL MODE
;; https://www.emacswiki.org/emacs/AutoFillMode
;; Type M-x auto-fill-mode to activate the MinorMode for the current buffer, or put the following in your .emacs to activate it for all text mode buffers:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(auto-fill-mode t)
;; END AUTO FILL MODE

;;;; Show trailing whitespace
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
;; This (for some reason) anly activates it only in the current buffer (not interactively)
;; > (setq show-trailing-whitespace t)
;; 
(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))
;;;; END Show trailing whitespace

;;(load "activate-markdown-mode.el")

;;
;; sly 
;;
(add-to-list 'load-path "~/.emacs.d/plugins/sly")
(require 'sly-autoloads)
;;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/sbcl")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;(custom-set-variables
;; ;; custom-set-variables was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(custom-safe-themes
;;   '("cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" default))
;; '(package-selected-packages '(use-package magit company auto-complete)))
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; )
