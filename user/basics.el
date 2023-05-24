;;
;; What can be used without any plugins
;;
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(global-hl-line-mode 1)
(global-linum-mode 1)



;; Recent files
;; https://emacs.stackexchange.com/questions/44589/how-show-recent-files
(require 'recentf)
(recentf-mode 1) ;; M-x recentf-open-files
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;


;; https://github.com/daviwil/emacs-from-scratch/blob/3075158cae210060888001c0d76a58a4178f6a00/init.el
;;;; http://ergoemacs.org/emacs/emacs_toggle-word-wrap.html
(setq tooggle-word-wrap t)
(setq-default fill-column 65)
;; Highlight selection
(transient-mark-mode t)
;;(set-face-attribute 'default nil :height 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

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
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(require 'ido)
(ido-mode t)

;; Set window size
(when window-system (set-frame-size (selected-frame) 100 40))
;; show column numbers
(setq column-number-mode t)
;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
;; See matching pairs of parentheses 
;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; MELPA PACKAGES SET UP
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("tromey" . "http://tromey.com/elpa/") t)
;; 
;; (package-initialize)

;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;; 
;; (setq package-list '(auto-complete magit))
;; 
;; ;; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ;; Automatically download packages if missing
;;
;; END MELPA PACKAGES SET UP
;;

;;
;; Keys
;;

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(global-set-key (kbd "C-z") 'goto-line) ;; Better use it for buffer centering
(global-set-key [S-up] 'backward-paragraph)      ;; Jump to previous paragraph
(global-set-key [S-down] 'forward-paragraph)     ;; Jump to next paragraph
;; Switch window with Ctrl-TAB 
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-x g") 'magit-status)
;;(global-set-key (kbd "<delete>") 'delete-region)

;; https://www.emacswiki.org/emacs/SearchAtPoint
;; Search symbols
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)
(global-set-key [f3] 'highlight-symbol-next)
;;(global-set-key [(ctrl q)] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)


;; AUTO FILL MODE
;; https://www.emacswiki.org/emacs/AutoFillMode
;; Type M-x auto-fill-mode to activate the MinorMode for the current buffer, or put the following in your .emacs to activate it for all text mode buffers:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(auto-fill-mode t)

;; Add new line without breaking the current line
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'end-of-line-and-indented-new-line)

;; iBufer: http://emacswiki.org/emacs/IbufferMode
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Override Ctrl-TAB in org mode
;; http://stackoverflow.com/questions/4333467/override-ctrl-tab-in-emacs-org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))


;; org mode: no empty line 
;; C-h v org-blank-before-new-entry 
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))



(setq backup-directory-alist
      `((".*" . , "~/.emacstemp/")))

(setq auto-save-file-name-transforms
      `((".*" , "~/.emacstemp/" t)))

;; User interface
;; Set default font
;; (set-default-font "Courier")
;;(set-default-font "Lucida Console")
;;(set-default-font "Courier")
;;(set-default-font "Consolas") ;; used in windows
;;;; Fira Code font: https://github.com/tonsky/FiraCode
;;(set-default-font "Fira Code") 



;; Calendar localization
;; http://www.emacswiki.org/emacs/CalendarLocalization
(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))
	    
;; FUNCTIONS
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

(defun ap ()
  (interactive)
  (auto-complete-mode t)
  (paredit-mode t))

;; Insert focus item 
(defun it ()
  (interactive)
  (insert(concat "* " (format-time-string "%Y-%m-%d")))
  (insert
"

| LA              | [ ] * 0 |
| Thesis / Papers | [ ] * 0 |
| Read Papers     | [ ] * 0 |

- One Haskell task
- ML lib: 5-10 min
"))
