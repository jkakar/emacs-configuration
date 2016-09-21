;; Load path

(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/local" load-path))
(setenv "GOPATH" (expand-file-name "~"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setq exec-path (append (list (expand-file-name "~/bin") "/usr/local/Cellar/go/1.4.2/bin") exec-path))

;; Standard packages

(require 'cl)
(require 'dired+)
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Local packages
;; (add-to-list 'load-path "~/src/github.com/dougm/goflymake")

;; Plugins

(load-library "markdown-mode.el")
(load-library "highlight-80+.el")
(load-library "less-css-mode.el")

;; (require 'go-flymake)

(require 'appearance)
(require 'interaction)
(require 'programming)
