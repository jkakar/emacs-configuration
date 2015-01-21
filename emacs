;; Load path

(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))
(setq load-path (cons "/usr/local/go/misc/emacs" load-path))
(setq load-path (cons "/usr/local/Cellar/go//1.2.1/libexec/misc/emacs" load-path))

;; Standard packages

(require 'cl)
(require 'dired+)
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Plugins

(require 'python-outline)
(load-library "markdown-mode.el")
(load-library "highlight-80+.el")
(load-library "less-css-mode.el")

(setenv "GOPATH" (expand-file-name "~"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setq exec-path (append (list (expand-file-name "~/bin") "/usr/local/go/bin") exec-path))
(setq gofmt-command "goimports")
(add-to-list 'load-path (expand-file-name "~/src/github.com/golang/lint/misc/emacs"))
(add-to-list 'load-path "~/src/github.com/dougm/goflymake")
(require 'go-flymake)
(require 'golint)

;;(add-hook 'after-init-hook 'my-after-init-hook)
;;(defun my-after-init-hook ()
;;  (require 'edts-start))

(require 'appearance)
(require 'interaction)
(require 'programming)

(require 'elixir-mode)
