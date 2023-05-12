;;
;; What can be used without any plugins
;;
(setq path-to-emacsd "~/.emacs.d/")

;; Turn off beep
(setq visible-bell 1) 
(require 'ido)
(ido-mode t)

(global-hl-line-mode 1)
(global-linum-mode 1)

;;
;; Key bindings
;;
(global-set-key (kbd "C-z") 'goto-line) ;; Better use it for buffer centering
(global-set-key [S-up] 'backward-paragraph)      ;; Jump to previous paragraph
(global-set-key [S-down] 'forward-paragraph)     ;; Jump to next paragraph
;; Switch window with Ctrl-TAB 
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-x G") 'magit-status)
(global-set-key (kbd "<delete>") 'delete-region)

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

;;;; http://ergoemacs.org/emacs/emacs_toggle-word-wrap.html
(setq tooggle-word-wrap t)
(setq-default fill-column 65)

;; org mode: no empty line 
;; C-h v org-blank-before-new-entry 
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq inhibit-startup-message t) ;; No splash screen
;;(setq initial-scratch-message nil) ;; No scratch message

(defun get-full-path (subpath)
  (concat path-to-emacsd subpath))

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


;; Disable tool- and menu- bars
(tool-bar-mode -1)
;;(menu-bar-mode -1)

;; Highlight selection
(transient-mark-mode t)

;; Uncomment this to increase font size
(set-face-attribute 'default nil :height 125)

;; Calendar localization
;; http://www.emacswiki.org/emacs/CalendarLocalization
(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))
	    
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
