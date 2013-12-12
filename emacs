;; Load path

(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))


;; Standard packages

(require 'cl)
(require 'dired+)

;; Plugins

;; (load-library "python-mode.el")
;; (require 'python-test)
(require 'python-outline)
;; (load-library "flymake-cursor")
(load-library "markdown-mode.el")
(load-library "highlight-80+.el")
(load-library "less-css-mode.el")

;; Local settings

(require 'appearance)
(require 'interaction)
(require 'programming)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(require 'elixir-mode)