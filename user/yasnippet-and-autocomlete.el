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
