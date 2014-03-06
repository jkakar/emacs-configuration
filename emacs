;; Load path


;; (setenv "GOPATH" (expand-file-name "$HOME/Projects/go/gopath"))
;; (setenv "PATH" "/Users/dan/bin:/opt/boxen/rbenv/bin:/opt/boxen/bin:/opt/boxen/homebrew/bin:/opt/boxen/homebrew/sbin:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Postgres.app/Contents/Versions/9.3/bin:/usr/local/bin:/usr/local/google-cloud-sdk/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/dan/Projects/go/gopath/bin:/usr/local/go/bin")
;; (setq exec-path (append (list (expand-file-name "~/Projects/go/gopath/bin") "/usr/local/go/bin" "/opt/boxen/rbenv/bin") exec-path))
 
;; ; I installed go-mode via package-install
;; ; I installed goflymake with `go get -u github.com/dougm/goflymake`
 
;; (add-to-list 'load-path "~/Projects/go/gopath/src/github.com/dougm/goflymake")
;; (require 'go-flymake)



(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))
(setq load-path (cons "/usr/local/go/misc/emacs" load-path))
(setq load-path (cons "/usr/local/Cellar/go//1.2.1/libexec/misc/emacs" load-path))


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

(setenv "GOPATH" (expand-file-name "~/src/go"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/src/go/bin")))
(setq exec-path (append (list (expand-file-name "~/src/go/bin") "/usr/local/go/bin") exec-path))
(setq gofmt-command "goimports")

(add-to-list 'load-path (expand-file-name "~/src/go/src/github.com/golang/lint/misc/emacs"))
(add-to-list 'load-path "~/src/go/src/github.com/dougm/goflymake")
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
