;; Widgets

(defun disable-bars ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun modeline-widgets ()
  (setq line-number-mode t)
  (setq column-number-mode t))

(defun whitespace-settings ()
  (setq default-tab-width 4))


;; Editing

(defun highlight-region ()
  (setq transient-mark-mode 1)
  (setq-default show-trailing-whitespace t)
  (setq highlight-80+-columns 79)
  (add-hook 'emacs-lisp-mode-hook 'highlight-80+-mode)
  (add-hook 'erlang-mode-hook 'highlight-80+-mode)
  (add-hook 'text-mode-hook 'highlight-80+-mode)
  (add-hook 'mail-mode-hook 'highlight-80+-mode)
  (add-hook 'html-mode-hook 'highlight-80+-mode)
  (add-hook 'python-mode-hook 'highlight-80+-mode)
  (add-hook 'sgml-mode-hook 'highlight-80+-mode)
  (add-hook 'c++-mode-hook 'highlight-80+-mode)
  (add-hook 'go-mode-hook 'highlight-80+-mode)
  (add-hook 'javascript-mode-hook 'highlight-80+-mode)
  (add-hook 'js2-mode-hook 'highlight-80+-mode)
  (add-hook 'java-mode-hook 'highlight-80+-mode)
  (add-hook 'html-mode-hook 'highlight-80+-mode)
  (add-hook 'shell-script-mode-hook 'highlight-80+-mode)
  (add-hook 'ruby-mode-hook 'highlight-80+-mode))

(defun scrolling-behaviour ()
  (setq scroll-conservatively 1))


;; Customize

(defun set-and-load-custom-file ()
  (setq custom-file "~/.emacs.d/local/custom.el")
  (load custom-file))


;; Load all settings

(disable-bars)
(modeline-widgets)
(highlight-region)
(set-and-load-custom-file)
(scrolling-behaviour)
(whitespace-settings)

(provide 'appearance)
