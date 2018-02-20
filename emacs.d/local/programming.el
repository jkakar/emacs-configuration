(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'after-init-hook 'global-company-mode)

;; Erlang

(add-hook 'erlang-mode-hook
      '(lambda () "Defaults for Erlang mode." (setq fill-column 78)))

;; Elixir

(add-hook 'elixir-mode-hook
      '(lambda () "Defaults for Elixir mode." (setq fill-column 78)))
(setq auto-mode-alist (cons '("\\.eex$" . web-mode) auto-mode-alist))

;; Go

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(add-hook 'go-mode-hook
      '(lambda () "Defaults for Go mode." (setq fill-column 78)))
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Python

(defun unset-python-newline-and-indent ()
  (local-unset-key [?\C-j]))

(add-hook 'python-mode-hook
      '(lambda () "Defaults for Python mode." (setq fill-column 78
                                                    python-guess-indent nil
                                                    python-indent 4)))
(add-hook 'python-mode-hook 'unset-python-newline-and-indent)
(setq auto-mode-alist (cons '("\\.tac$" . python-mode) auto-mode-alist))
(setq tags-table-list-default '("~/.etags/twisted" "~/.etags/txamqp"))

;; Configuration

(setq auto-mode-alist (cons '("muttrc" . conf-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".offlineimaprc" . conf-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".msmtprc" . conf-mode) auto-mode-alist))

;; Ruby

(add-hook 'ruby-mode-hook
      '(lambda () "Defaults for Ruby mode." (setq fill-column 78)))
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gemspec$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))


;; CoffeeScript

(add-hook 'coffee-mode-hook
      '(lambda () "Defaults for Coffee mode." (setq fill-column 78)))


;; Javascript

(setq javascript-mode-hook
      '(lambda () "Defaults for Javascript mode." (setq fill-column 78)))


;; Restructured text

(setq rst-mode-hook
      '(lambda () "Defaults for ReST mode." (setq fill-column 78)))


;; C

(setq-default c-basic-offset 4)

;; C++

(setq c++-mode-hook
      '(lambda () "Defaults for C++ mode." (setq fill-column 78)))
(c-set-offset 'substatement-open 0)


;; XML

(setq auto-mode-alist (cons '("\\.erb$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rdf$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xul$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . xml-mode) auto-mode-alist))


;; org-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; Markdown

(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))
(setq markdown-mode-hook
      '(lambda () "Defaults for Markdown mode." (setq fill-column 78)))


;; Scala

(setq auto-mode-alist (cons '("\\.sbt$" . scala-mode) auto-mode-alist))


;; Mail

;; (setq auto-mode-alist (cons '("mail\\.google\\.com." . ham-mode) auto-mode-alist))
;; (setq ham-mode-hook
;;       '(lambda () "Defaults for ham mode." (setq fill-column 68)))


;; Docker

(setq auto-mode-alist (cons '("Dockerfile" . conf-mode) auto-mode-alist))

;; Dumb jump

(dumb-jump-mode)
(setq dumb-jump-prefer-searcher 'rg)

(provide 'programming)
