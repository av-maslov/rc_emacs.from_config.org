(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
  (use-package evil-tutor)

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
;; (elpaca nil (message "deferred"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer al/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (al/leader-keys
    "." '(ido-dired :wk "Dired")
    ;;"." '(ibuffer :wk "ibuffer")
    "," '(ivy-switch-buffer :wk "ivy switch buffer")
    ;;"." '(ibuffer :wk "ibuffer")
    ;;"f" '(find-file :wk "Find file")
    "f" '(counsel-find-file :wk "Find file")
    "w" '(save-buffer :wk "Save buffer")
    "b" '(counsel-bookmark :wk "Bookrmark jump")
    ;;"b" '(bookmark-jump :wk "Bookrmark jump")
    ;;"l" '(bookmark-bmenu-list :wk "Show bookmarks")
    "g c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  ;; (al/leader-keys
  ;;   "w" '(:ignore t :wk "Window")
  ;;   "w v" '(split-window-right :wk "Split vertical")
  ;;   "w h" '(split-window-below :wk "Split below")
  ;;   )

  ;; (al/leader-keys
  ;;   "b" '(:ignore t :wk "buffer")
  ;;   "b b" '(switch-to-buffer :wk "Switch buffer")
  ;;   "b i" '(ibuffer :wk "Ibuffer")
  ;;   "b k" '(kill-this-buffer :wk "Kill this buffer")
  ;;   "b n" '(next-buffer :wk "Next buffer")
  ;;   "b p" '(previous-buffer :wk "Previous buffer")
  ;;   "b r" '(revert-buffer :wk "Reload buffer"))

 (al/leader-keys
  "e" '(:ignore t :wk "Evaluate")    
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate and elisp expression")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region"))

 (al/leader-keys  
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h v" '(describe-variable :wk "Describe variable")
  ;;"h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))
  "h r r" '(reload-init-file :wk "Reload emacs config"))

 (al/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t t" '(visual-line-mode :wk "Toggle truncated lines"))
 )

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(require 'org-tempo)

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
  which-key-sort-order #'which-key-key-order-alpha
  which-key-sort-uppercase-first nil
  which-key-add-column-padding 1
  which-key-max-display-columns nil
  which-key-min-display-lines 6
  which-key-side-window-slot -10
  which-key-side-window-max-height 0.25
  which-key-idle-delay 0.8
  which-key-max-description-length 25
  which-key-allow-imprecise-window-fit t
  which-key-separator " → " ))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode 1)          ; menu bar
(global-hl-line-mode -1)    ; Show current line
;; (global-linum-mode 1)
;; Recent files
;; https://emacs.stackexchange.com/questions/44589/how-show-recent-files
(require 'recentf)
(recentf-mode 1) ;; M-x recentf-open-files
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(add-to-list 'recentf-exclude "/home/al/.emacs.d/") ;; M-x recentf-cleanup will update your recentf-list
;; END: recent files
;; https://github.com/daviwil/emacs-from-scratch/blob/3075158cae210060888001c0d76a58a4178f6a00/init.el
;;;; http://ergoemacs.org/emacs/emacs_toggle-word-wrap.html
(setq tooggle-word-wrap t)
(setq-default fill-column 65)
;; Highlight selection
(transient-mark-mode t)
;;(set-face-attribute 'default nil :height 125)
;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
(setq history-length 25)
(savehist-mode 1)
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Turn off beep
;; (setq visible-bell 1)
;; Set up the visible bell
(setq visible-bell t)
(setq inhibit-startup-message t) ;; No splash screen
;;(setq initial-scratch-message nil) ;; No scratch message
(require 'ido)
(ido-mode t)
;; Set window size
;; (when window-system (set-frame-size (selected-frame) 100 40))
;; show column numbers
(setq column-number-mode t)
;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
;; See matching pairs of parentheses 
;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq display-line-numbers-type 'relative)
;; org mode: no empty line 
;; C-h v org-blank-before-new-entry 
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq backup-directory-alist
      `((".*" . , "~/.emacstemp/")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacstemp/" t)))

(use-package sly)
(load "~/.emacs.d/user/sly.el")
;;(load "~/.emacs.d/user/yasnippet-and-autocomlete.el")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-z") 'goto-line) ;; Better use it for buffer centering
(global-set-key [S-up] 'backward-paragraph)      ;; Jump to previous paragraph
(global-set-key [S-down] 'forward-paragraph)     ;; Jump to next paragraph

;; Switch window with Ctrl-TAB 
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-x g") 'magit-status)

;;(load "~/.emacs.d/init_minimum_from_scratch.el")
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-one t)
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(load-theme 'deeper-blue)
;;(load-theme 'dracula-theme)
;;(load-theme 'wombat)
;;(load-theme 'doom-dracula)

(use-package tree-sitter
:ensure t 
)
(use-package tree-sitter-langs
:ensure t
)
;;(global-tree-sitter-mode)
;; (add-hook 'rust-mode-hook #'tree-sitter-mode)
;; (add-hook 'python-mode-hook #'tree-sitter-mode)
;; (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
;; (tree-sitter-hl-mode 1)

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

(use-package rg
  :config
  (rg-enable-default-bindings) ;;  C-c s r (rg)
  )

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env ".venv/bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;(add-to-list 'load-path "~/git/swiper/")
;;(require 'ivy)
(use-package counsel
:ensure t
:config

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c m") 'counsel-linux-app)

(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)

(global-set-key (kbd "C-c F") 'counsel-org-file)
)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  ;; Remember candidate frequencies across sessions
  (prescient-persist-mode 1)
  )

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config 
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
  )

(use-package auto-complete
  :ensure t
  :config 
(ac-config-default)
(auto-complete-mode t)
(global-auto-complete-mode t)

  )

;;(use-package auto-complete-config
;;  :ensure t)
