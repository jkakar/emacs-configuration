;; Load path

(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))
(setq load-path (cons "/usr/local/go/misc/emacs" load-path))


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

(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "HOME") "/src/go/bin")))

(add-to-list 'load-path (concat (getenv "HOME")  "/src/go/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
(setq gofmt-command "goimports")
(require 'go-mode-load)

(require 'appearance)
(require 'interaction)
(require 'programming)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(require 'elixir-mode)
