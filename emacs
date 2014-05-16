;; Load path

(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))
(setq load-path (cons "/usr/local/go/misc/emacs" load-path))
(setq load-path (cons "/usr/local/Cellar/go//1.2.1/libexec/misc/emacs" load-path))
(setq load-path (cons "~/src/code.google.com/p/go.tools/cmd/oracle" load-path))

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
(load-library "oracle.el")

;; Local settings

(setenv "GOPATH" (expand-file-name "~"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setq exec-path (append (list (expand-file-name "~/bin") "/usr/local/go/bin") exec-path))
(setq gofmt-command "goimports")

(add-to-list 'load-path (expand-file-name "~/src/github.com/golang/lint/misc/emacs"))
(add-to-list 'load-path "~/src/github.com/dougm/goflymake")
(require 'go-flymake)
(require 'golint)
(require 'go-mode-load)

(require 'appearance)
(require 'interaction)
(require 'programming)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(require 'elixir-mode)
