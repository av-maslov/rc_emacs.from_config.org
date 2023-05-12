(require 'whitespace)
;; highlight long lines
;; http://www.emacswiki.org/emacs/HighlightLongLines
(setq whitespace-style '(lines))
(setq whitespace-line-column 80)
(global-whitespace-mode 1)
(setq whitespace-style '(tabs trailing lines tab-mark))
